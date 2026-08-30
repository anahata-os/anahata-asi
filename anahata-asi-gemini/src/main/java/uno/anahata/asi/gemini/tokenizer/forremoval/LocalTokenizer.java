package uno.anahata.asi.gemini.tokenizer;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
import com.google.genai.proto.SentencepieceModel.ModelProto;
import com.google.genai.proto.SentencepieceModel.ModelProto.SentencePiece;
import com.google.genai.types.ComputeTokensResult;
import com.google.genai.types.Content;
import com.google.genai.types.CountTokensConfig;
import com.google.genai.types.CountTokensResult;
import com.google.genai.types.FunctionCall;
import com.google.genai.types.FunctionDeclaration;
import com.google.genai.types.FunctionResponse;
import com.google.genai.types.Part;
import com.google.genai.types.Schema;
import com.google.genai.types.TokensInfo;
import com.google.genai.types.Tool;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public final class LocalTokenizer {
  private final ModelProto modelProto;
  private final LocalTokenizerProcessor tokenizer;

  public LocalTokenizer(String modelName) {
    String tokenizerName = LocalTokenizerLoader.getTokenizerName(modelName);
    this.modelProto = LocalTokenizerLoader.loadModelProto(tokenizerName);
    this.tokenizer = new LocalTokenizerProcessor(this.modelProto);
  }

  @VisibleForTesting
  LocalTokenizer(ModelProto modelProto, LocalTokenizerProcessor tokenizer) {
    this.modelProto = modelProto;
    this.tokenizer = tokenizer;
  }

  private static List<Content> tContents(Object obj) {
    if (obj instanceof String) {
      return java.util.Collections.singletonList(Content.builder().parts(java.util.Collections.singletonList(Part.builder().text((String)obj).build())).role("user").build());
    } else if (obj instanceof Content) {
      return java.util.Collections.singletonList((Content) obj);
    } else if (obj instanceof List) {
      return (List<Content>) obj;
    }
    throw new IllegalArgumentException("Unsupported type for tContents: " + obj.getClass());
  }

  public CountTokensResult countTokens(List<Content> contents, CountTokensConfig config) {
    List<Content> processedContents = contents;
    TextsAccumulator textAccumulator = new TextsAccumulator();

    if (config == null) {
      config = CountTokensConfig.builder().build();
    }

    textAccumulator.addContents(processedContents);
    if (config.tools().isPresent()) {
      textAccumulator.addTools(config.tools().get());
    }
    if (config.generationConfig().isPresent()
        && config.generationConfig().get().responseSchema().isPresent()) {
      textAccumulator.addSchema(config.generationConfig().get().responseSchema().get());
    }
    if (config.systemInstruction().isPresent()) {
      textAccumulator.addContents(tContents(config.systemInstruction().get()));
    }

    int totalTokens = 0;
    for (String text : textAccumulator.getTexts()) {
      totalTokens += tokenizer.encode(text).size();
    }

    return CountTokensResult.builder().totalTokens(totalTokens).build();
  }

  public CountTokensResult countTokens(List<Content> contents) {
    return countTokens(contents, null);
  }

  public CountTokensResult countTokens(Content content, CountTokensConfig config) {
    return countTokens(ImmutableList.of(content), config);
  }

  public CountTokensResult countTokens(Content content) {
    return countTokens(content, null);
  }

  public CountTokensResult countTokens(String content, CountTokensConfig config) {
    return countTokens(tContents(content), config);
  }

  public CountTokensResult countTokens(String content) {
    return countTokens(content, null);
  }

  public ComputeTokensResult computeTokens(List<Content> contents) {
    List<Content> processedContents = contents;
    List<TokensInfo> tokenInfos = new ArrayList<>();

    for (Content content : processedContents) {
      if (content.parts().isPresent()) {
        for (Part part : content.parts().get()) {
          TextsAccumulator partAccumulator = new TextsAccumulator();
          partAccumulator.addPart(part);

          List<Long> allTokenIds = new ArrayList<>();
          List<byte[]> allTokenBytes = new ArrayList<>();

          for (String text : partAccumulator.getTexts()) {
            List<Token> tokens = tokenizer.encode(text);
            for (Token token : tokens) {
              allTokenIds.add((long) token.id());
              allTokenBytes.add(
                  tokenStrToBytes(token.text(), modelProto.getPieces(token.id()).getType()));
            }
          }

          tokenInfos.add(
              TokensInfo.builder()
                  .tokenIds(allTokenIds)
                  .tokens(allTokenBytes)
                  .role(content.role().orElse(null))
                  .build());
        }
      }
    }
    return ComputeTokensResult.builder().tokensInfo(tokenInfos).build();
  }

  public ComputeTokensResult computeTokens(Content content) {
    return computeTokens(ImmutableList.of(content));
  }

  public ComputeTokensResult computeTokens(String content) {
    return computeTokens(tContents(content));
  }

  private byte[] tokenStrToBytes(String token, SentencePiece.Type type) {
    if (type == SentencePiece.Type.BYTE) {
      return new byte[] {(byte) parseHexByte(token)};
    } else {
      return token.replace('\u2581', ' ').getBytes(StandardCharsets.UTF_8);
    }
  }

  private int parseHexByte(String token) {
    if (token.length() != 6 || !token.startsWith("<0x") || !token.endsWith(">")) {
      throw new IllegalArgumentException("Invalid byte format: " + token);
    }
    try {
      int val = Integer.parseInt(token.substring(3, 5), 16);
      if (val >= 256) {
        throw new IllegalArgumentException("Byte value out of range: " + token);
      }
      return val;
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException("Invalid hex value: " + token, e);
    }
  }

  private static class TextsAccumulator {
    private static final Logger logger = Logger.getLogger(TextsAccumulator.class.getName());
    private final List<String> texts = new ArrayList<>();

    public List<String> getTexts() {
      return texts;
    }

    public void addContents(List<Content> contents) {
      for (Content content : contents) {
        addContent(content);
      }
    }

    public void addContent(Content content) {
      Content countedContent = addContentAndBuildCounted(content);
      if (!Objects.equals(content, countedContent)) {
        logger.warning(
            "Content contains unsupported types for token counting. Supported fields "
                + countedContent
                + ". Got "
                + content
                + ".");
      }
    }

    private Content addContentAndBuildCounted(Content content) {
      Content.Builder countedContentBuilder = Content.builder();
      content.role().ifPresent(countedContentBuilder::role);

      if (content.parts().isPresent()) {
        List<Part> countedParts =
            content.parts().get().stream()
                .map(this::addPartAndBuildCounted)
                .collect(Collectors.toList());
        countedContentBuilder.parts(countedParts);
      }
      return countedContentBuilder.build();
    }

    protected void addPart(Part part) {
      addPartAndBuildCounted(part);
    }

    private Part addPartAndBuildCounted(Part part) {
      Part.Builder countedPartBuilder = Part.builder();
      if (part.fileData().isPresent() || part.inlineData().isPresent()) {
        throw new IllegalArgumentException(
            "LocalTokenizers do not support non-text content types.");
      }
      part.videoMetadata().ifPresent(countedPartBuilder::videoMetadata);
      part.functionCall()
          .ifPresent(
              fc -> {
                addFunctionCall(fc);
                countedPartBuilder.functionCall(fc);
              });
      part.functionResponse()
          .ifPresent(
              fr -> {
                addFunctionResponse(fr);
                countedPartBuilder.functionResponse(fr);
              });
      part.text()
          .ifPresent(
              text -> {
                texts.add(text);
                countedPartBuilder.text(text);
              });
      return countedPartBuilder.build();
    }

    public void addFunctionCall(FunctionCall functionCall) {
      functionCall.name().ifPresent(texts::add);
      functionCall.args().ifPresent(this::traverseMap);
    }

    public void addFunctionResponse(FunctionResponse functionResponse) {
      functionResponse.name().ifPresent(texts::add);
      functionResponse.response().ifPresent(this::traverseMap);
    }

    public void addTools(List<Tool> tools) {
      for (Tool tool : tools) {
        addTool(tool);
      }
    }

    public void addTool(Tool tool) {
      if (tool.functionDeclarations().isPresent()) {
        for (FunctionDeclaration functionDeclaration : tool.functionDeclarations().get()) {
          addFunctionDeclaration(functionDeclaration);
        }
      }
    }

    private void addFunctionDeclaration(FunctionDeclaration functionDeclaration) {
      functionDeclaration.name().ifPresent(texts::add);
      functionDeclaration.description().ifPresent(texts::add);
      functionDeclaration.parameters().ifPresent(this::addSchema);
    }

    public void addSchema(Schema schema) {
      schema.format().ifPresent(texts::add);
      schema.description().ifPresent(texts::add);
      schema.enum_().ifPresent(texts::addAll);
      schema.required().ifPresent(texts::addAll);
      schema.items().ifPresent(this::addSchema);
      if (schema.properties().isPresent()) {
        for (Map.Entry<String, Schema> entry : schema.properties().get().entrySet()) {
          texts.add(entry.getKey());
          addSchema(entry.getValue());
        }
      }
      schema.example().ifPresent(this::traverseObject);
    }

    private void traverseObject(Object value) {
      if (value instanceof String) {
        texts.add((String) value);
      } else if (value instanceof Map) {
        @SuppressWarnings("unchecked")
        Map<String, Object> map = (Map<String, Object>) value;
        traverseMap(map);
      } else if (value instanceof List) {
        for (Object item : (List<?>) value) {
          traverseObject(item);
        }
      }
    }

    private void traverseMap(Map<String, Object> map) {
      for (Map.Entry<String, Object> entry : map.entrySet()) {
        texts.add(entry.getKey());
        traverseObject(entry.getValue());
      }
    }
  }
}