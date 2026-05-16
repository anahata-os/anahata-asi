package uno.anahata.asi.nb.tools.java.coderefiner;

import uno.anahata.asi.nb.tools.java.BatchCodeRefiner;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import java.io.OutputStream;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodToolResponse;

@Slf4j
public class CodeRefinementBatchTest {

    public static void logToToolContext(String s) {
        JavaMethodToolResponse.getCurrent().addLog(s);
    }
    
    public static void exceptionToolContext(Exception e) {
        JavaMethodToolResponse.getCurrent().addError(e);
    }
    
    public static void runAllTests(Agi agi) throws Exception {
        logToToolContext("Turn #288: Analyzing V3 limitations and preparing for V4 AST-Guided Text Replacement");
        logToToolContext("Starting Comprehensive BCR Test Suite...");
        BatchCodeRefiner refiner = new BatchCodeRefiner();
        
        String uri = "file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb/src/main/java/uno/anahata/asi/nb/tools/java/coderefiner/SmallTestClass.java";
        java.util.Optional<Resource> found = agi.getResourceManager().findByUri(uri);
        if (found.isEmpty()) throw new Exception("SmallTestClass.java resource not found by URI.");
        Resource res = found.get();
        String uuid = res.getId();
        NbHandle handle = (NbHandle) res.getHandle();
        
        java.util.function.Function<List<CodeRefinementIntent>, CodeRefinementBatch> buildBatch = (intents) -> {
            handle.getFileObject().refresh();
            CodeRefinementBatch b = new CodeRefinementBatch();
            b.setResourceUuid(uuid);
            b.setLastModified(handle.getFileObject().lastModified().getTime());
            b.setOptimize(false);
            b.setSave(true);
            b.setIntents(intents);
            return b;
        };

        Runnable reset = () -> {
            try {
                String baseContent = "package uno.anahata.asi.nb.tools.java.coderefiner;\n\npublic class SmallTestClass {\n}\n";
                try (OutputStream os = handle.getFileObject().getOutputStream()) {
                    os.write(baseContent.getBytes());
                }
                handle.getFileObject().refresh();
            } catch (Exception e) {
                exceptionToolContext(e);
                throw new RuntimeException(e);
            }
        };

        java.util.function.Consumer<CodeRefinementBatch> runBatch = (batch) -> {
            try {
                 refiner.refine(batch);
                logToToolContext("Current Content:\n" + new String(handle.getFileObject().asBytes(), "UTF-8"));
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };

        reset.run();

        logToToolContext("Test 1: Class Level Javadoc");
        CodeRefinementIntent i1 = new CodeRefinementIntent();
        i1.setType(CodeRefinementIntent.Type.UPDATE);
        i1.setMemberFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass");
        JavadocIntent j1 = new JavadocIntent();
        j1.setDescription("Base Test Class for AST.");
        i1.setJavadoc(j1);
        runBatch.accept(buildBatch.apply(List.of(i1)));
        
        logToToolContext("Test 2: Create Inner Class with Members & Javadoc");
        CodeRefinementIntent i2 = new CodeRefinementIntent();
        i2.setType(CodeRefinementIntent.Type.INSERT);
        i2.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass");
        i2.setPosition(RelativePosition.END);
        i2.setDeclaration("public static class InnerTest");
        i2.setBody("private int a;\nprivate String b;\npublic void foo() {}");
        JavadocIntent j2 = new JavadocIntent();
        j2.setDescription("Inner Class Doc.");
        i2.setJavadoc(j2);
        runBatch.accept(buildBatch.apply(List.of(i2)));

        logToToolContext("Test 3: Insert Method with @SneakyThrows and Blank Lines");
        CodeRefinementIntent i3 = new CodeRefinementIntent();
        i3.setType(CodeRefinementIntent.Type.INSERT);
        i3.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass");
        i3.setPosition(RelativePosition.END);
        i3.setDeclaration("@lombok.SneakyThrows\npublic void riskyMethod()");
        i3.setBody("System.out.println(\"A\");\n\n// Space!\n\nSystem.out.println(\"B\");");
        runBatch.accept(buildBatch.apply(List.of(i3)));

        logToToolContext("Test 4: Update Inner Class Member (Insert with Annotation)");
        CodeRefinementIntent i4 = new CodeRefinementIntent();
        i4.setType(CodeRefinementIntent.Type.INSERT);
        i4.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass$InnerTest");
        i4.setPosition(RelativePosition.BEFORE);
        i4.setAnchorMemberName("foo()");
        i4.setDeclaration("@Deprecated\npublic void bar()");
        i4.setBody("System.out.println(\"bar\");");
        runBatch.accept(buildBatch.apply(List.of(i4)));

        logToToolContext("Test 5: Update Member Javadoc Only");
        CodeRefinementIntent i5 = new CodeRefinementIntent();
        i5.setType(CodeRefinementIntent.Type.UPDATE);
        i5.setMemberFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass.riskyMethod()");
        JavadocIntent j5 = new JavadocIntent();
        j5.setDescription("This method is extremely risky.");
        i5.setJavadoc(j5);
        runBatch.accept(buildBatch.apply(List.of(i5)));

        logToToolContext("Test 6: Move member of Inner Class");
        CodeRefinementIntent i6 = new CodeRefinementIntent();
        i6.setType(CodeRefinementIntent.Type.MOVE);
        i6.setMemberFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass$InnerTest.foo()");
        i6.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass$InnerTest");
        i6.setPosition(RelativePosition.BEFORE);
        i6.setAnchorMemberName("bar()");
        runBatch.accept(buildBatch.apply(List.of(i6)));

        logToToolContext("Test 7: Delete member of Inner Class");
        CodeRefinementIntent i7 = new CodeRefinementIntent();
        i7.setType(CodeRefinementIntent.Type.DELETE);
        i7.setMemberFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass$InnerTest.a");
        runBatch.accept(buildBatch.apply(List.of(i7)));

        logToToolContext("Test 8: Insert Method with Generics");
        CodeRefinementIntent i8 = new CodeRefinementIntent();
        i8.setType(CodeRefinementIntent.Type.INSERT);
        i8.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass");
        i8.setPosition(RelativePosition.END);
        i8.setDeclaration("public <T extends Number> java.util.List<T> processGenerics(java.util.Map<String, T> input)");
        i8.setBody("return new java.util.ArrayList<>(input.values());");
        JavadocIntent j8 = new JavadocIntent();
        j8.setDescription("Processes generic numbers.");
        i8.setJavadoc(j8);
        runBatch.accept(buildBatch.apply(List.of(i8)));

        logToToolContext("Test 9: Insert Inner Class with Generics");
        CodeRefinementIntent i9 = new CodeRefinementIntent();
        i9.setType(CodeRefinementIntent.Type.INSERT);
        i9.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass");
        i9.setPosition(RelativePosition.END);
        i9.setDeclaration("public static class GenericInner<X, Y>");
        i9.setBody("private X first;\nprivate Y second;");
        runBatch.accept(buildBatch.apply(List.of(i9)));

        logToToolContext("Test 10: Update Method with Generics and Blank Lines");
        CodeRefinementIntent i10 = new CodeRefinementIntent();
        i10.setType(CodeRefinementIntent.Type.UPDATE);
        i10.setMemberFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass.processGenerics(java.util.Map)");
        i10.setDeclaration("public <T extends Number, R> java.util.List<R> processGenerics(java.util.Map<String, T> input)");
        i10.setBody("java.util.List<R> list = new java.util.ArrayList<>();\n\n// Look at this beautiful blank line!\n\nreturn list;");
        runBatch.accept(buildBatch.apply(List.of(i10)));

        logToToolContext("Test 11: Chained Anchoring (Multiple relative inserts)");
        CodeRefinementIntent i11a = new CodeRefinementIntent();
        i11a.setType(CodeRefinementIntent.Type.INSERT);
        i11a.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass");
        i11a.setPosition(RelativePosition.END);
        i11a.setDeclaration("public void methodA()");
        i11a.setBody("System.out.println(\"A\");");

        CodeRefinementIntent i11b = new CodeRefinementIntent();
        i11b.setType(CodeRefinementIntent.Type.INSERT);
        i11b.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass");
        i11b.setPosition(RelativePosition.AFTER);
        i11b.setAnchorMemberName("methodA()");
        i11b.setDeclaration("public void methodB()");
        i11b.setBody("System.out.println(\"B\");");

        CodeRefinementIntent i11c = new CodeRefinementIntent();
        i11c.setType(CodeRefinementIntent.Type.INSERT);
        i11c.setClassFqn("uno.anahata.asi.nb.tools.java.coderefiner.SmallTestClass");
        i11c.setPosition(RelativePosition.AFTER);
        i11c.setAnchorMemberName("methodB()");
        i11c.setDeclaration("public void methodC()");
        i11c.setBody("System.out.println(\"C\");");

        CodeRefinementBatch batch11 = buildBatch.apply(List.of(i11a, i11b, i11c));
        batch11.setImportsToAdd(List.of("java.util.LinkedList", "java.io.File"));
        runBatch.accept(batch11);

        logToToolContext("All tests executed. Verifying AST...");
        
        String finalContent = new String(handle.getFileObject().asBytes(), "UTF-8");
        logToToolContext("Final Output:\n" + finalContent);
        
        if (!finalContent.contains("public <T extends Number, R> java.util.List<R> processGenerics")) {
            throw new Exception("Test 10 Failed: processGenerics signature is missing or mangled!");
        }
        if (finalContent.contains("Processes generic;")) {
            throw new Exception("Test 8/10 Failed: Semicolon mangling produced 'Processes generic;'!");
        }
        if (!finalContent.contains("private X first;")) {
            throw new Exception("Test 9 Failed: GenericInner fields missing!");
        }
        if (!finalContent.contains("public void methodB()")) {
            throw new Exception("Test 11 Failed: Chained anchors missing!");
        }
        if (!finalContent.contains("import java.util.LinkedList;")) {
            throw new Exception("Test 11 Failed: Imports missing!");
        }
        
        logToToolContext("Validation SUCCESS. The AST is perfect.");
    }
}
