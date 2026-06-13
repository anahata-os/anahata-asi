/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.desktop;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.UserMessage;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.desktop.swing.AsiDesktopAsiContainer;
import uno.anahata.asi.openai.OpenAiResponsesProvider;

/**
 * A manual test script to verify the OpenAI provider's blocking and streaming 
 * capabilities within the Desktop container environment.
 */
@Slf4j
public class OpenAiManualTest {

    /**
     * The main entry point to execute the manual OpenAI integration test suite.
     * @param args The command line arguments.
     * @throws java.lang.Exception if any connection, execution, or streaming error occurs.
     */
    public static void main(String[] args) throws Exception {
        System.out.println("--- Starting OpenAI Manual Test ---");

        // 1. Initialize the Desktop Container
        AsiDesktopAsiContainer container = new AsiDesktopAsiContainer();
        
        // 2. Get the OpenAI Provider
        OpenAiResponsesProvider provider = (OpenAiResponsesProvider) container.getProvider("OpenAI");
        if (provider == null) {
            System.err.println("OpenAI Provider not found! Make sure it is registered in the container.");
            return;
        }

        // 3. List and select a model
        List<? extends AbstractModel> models = provider.listModels();
        AbstractModel model = models.stream()
                .filter(m -> "gpt-4o".equals(m.getModelId()))
                .findFirst()
                .orElse(null);

        if (model == null) {
            System.err.println("Model 'gpt-4o' not found among the " + models.size() + " models discovered for OpenAI.");
            System.err.println("Check your API key and permissions in ~/.anahata/asi/OpenAI/api_keys.txt");
            return;
        }
        System.out.println("Selected Model: " + model.getModelId());

        // 4. Setup AGI Session
        AgiConfig config = container.createNewAgiConfig();
        config.setSelectedProviderUuid(provider.getUuid());
        config.setSelectedModelId(model.getModelId());
        Agi agi = container.createNewAgi(config);

        // --- TEST 1: Blocking generateContent ---
        System.out.println("\n[Test 1] Blocking generateContent...");
        UserMessage blockingMsg = new UserMessage(agi);
        blockingMsg.addTextPart("hi, this is a blocking test. keep it short.");
        
        GenerationRequest blockingRequest = new GenerationRequest(agi.getRequestConfig(), List.of(blockingMsg));
        Response<? extends AbstractModelMessage> blockingResponse = model.generateContent(blockingRequest);
        
        System.out.println("Response Status: " + blockingResponse.getTotalTokenCount() + " tokens used.");
        blockingResponse.getCandidates().forEach(c -> {
            System.out.println("Model Response: " + c.asText(false));
        });

        System.out.println("\n[RAW JSON - BLOCKING]");
        System.out.println("--- Entire Response JSON ---");
        System.out.println(blockingResponse.getRawJson());
        System.out.println("--- Request Config JSON (SI & Tools) ---");
        System.out.println(blockingResponse.getRawRequestConfigJson());
        System.out.println("--- History JSON (User & Model) ---");
        System.out.println(blockingResponse.getRawHistoryJson());

        // --- TEST 2: Streaming generateContentStream ---
        System.out.println("\n[Test 2] Streaming generateContentStream...");
        UserMessage streamMsg = new UserMessage(agi);
        streamMsg.addTextPart("hi, this is a streaming test. tell me a very short joke.");
        
        GenerationRequest streamRequest = new GenerationRequest(agi.getRequestConfig(), List.of(streamMsg));
        CountDownLatch latch = new CountDownLatch(1);

        try {
            model.generateContentStream(streamRequest, new StreamObserver<Response<? extends AbstractModelMessage>>() {
                private List<? extends AbstractModelMessage> targets;

                @Override
                public void onStart(List<? extends AbstractModelMessage> candidates) {
                    this.targets = candidates;
                    System.out.print("Stream started: ");
                }

                @Override
                public void onNext(Response<? extends AbstractModelMessage> value) {
                    // In streaming, we print the latest candidate's content
                    if (!value.getCandidates().isEmpty()) {
                        System.out.print(value.getCandidates().get(0).asText(false));
                        System.out.flush();
                    }
                }

                @Override
                public void onError(Throwable t) {
                    System.err.println("\nStream Error: " + t.getMessage());
                    t.printStackTrace();
                    latch.countDown();
                }

                @Override
                public void onComplete() {
                    System.out.println("\nStream completed successfully.");
                    
                    if (targets != null && !targets.isEmpty()) {
                        Response<?> finalRes = targets.get(0).getResponse();
                        if (finalRes != null) {
                            System.out.println("\n[RAW JSON - STREAMING]");
                            System.out.println("--- Entire Response JSON ---");
                            System.out.println(finalRes.getRawJson());
                            System.out.println("--- Request Config JSON (SI & Tools) ---");
                            System.out.println(finalRes.getRawRequestConfigJson());
                            System.out.println("--- History JSON (User & Model) ---");
                            System.out.println(finalRes.getRawHistoryJson());
                        }
                    }
                    
                    latch.countDown();
                }
            });
        } catch (UnsupportedOperationException uoe) {
            System.out.println("Streaming test skipped: " + uoe.getMessage());
            latch.countDown();
        }

        latch.await();
        System.out.println("\n--- OpenAI Manual Test Finished ---");
        System.exit(0);
    }
}
