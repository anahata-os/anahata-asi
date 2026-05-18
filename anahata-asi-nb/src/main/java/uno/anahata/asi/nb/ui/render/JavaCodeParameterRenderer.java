/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.io.File;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.nb.tools.java.NbJava;
import uno.anahata.asi.swing.agi.message.part.tool.param.ObjectToStringParameterRenderer;

/**
 * A specialized parameter renderer that evaluates the correct context classpath 
 * for dynamic Java execution snippets and injects it into the NetBeans viewer.
 * 
 * @author anahata
 */
@Slf4j
public class JavaCodeParameterRenderer extends ObjectToStringParameterRenderer {

    /**
     * sets the language to 'java' and 'editable' to true.
     */
    public JavaCodeParameterRenderer() {
        super();
        setLanguage("java");
        setEditable(true);
    }

    /**
     * Overrides the handle factory to inject the correct context classpath for
     * dynamic Java execution snippets so the high-fidelity NetBeans viewer
     * can provide accurate semantic highlighting.
     * 
     * @param content The string content.
     * @param call The tool call.
     * @param paramName The parameter name.
     * @return The configured StringHandle.
     */
    @Override
    protected StringHandle createHandle(String content, AbstractToolCall<?, ?> call, String paramName) {
        StringHandle sh = super.createHandle(content, call, paramName);
        
        String cp = null;
        try {
            NbJava nbJava = agiPanel.getAgi().getToolManager().getToolkitInstance(NbJava.class).orElse(null);
            if (nbJava != null) {
                String toolName = call.getToolName();
                String defaultCp = nbJava.getDefaultClasspath();
                
                if (toolName.endsWith("compileAndExecuteInProject")) {
                    Map<String, Object> args = call.getEffectiveArgs();
                    String projectPath = (String) args.get("projectPath");
                    Boolean incDeps = (Boolean) args.get("includeDependencies");
                    Boolean incTestDeps = (Boolean) args.get("includeTestDependencies");
                    
                    if (projectPath != null) {
                        String projectCp = nbJava.buildProjectClasspathString(projectPath, Boolean.TRUE.equals(incDeps), Boolean.TRUE.equals(incTestDeps));
                        cp = projectCp + File.pathSeparator + defaultCp;
                    }
                } else if (toolName.endsWith("compileAndExecute")) {
                    Map<String, Object> args = call.getEffectiveArgs();
                    String extraCp = (String) args.get("extraClassPath");
                    if (extraCp != null && !extraCp.isEmpty()) {
                        cp = extraCp + File.pathSeparator + defaultCp;
                    } else {
                        cp = defaultCp;
                    }
                }
            }
        } catch (Exception e) {
            log.error("Failed to calculate custom classpath for JavaCodeParameterRenderer", e);
        }
        
        if (cp != null) {
            log.debug("Injecting custom classpath for parameter {} in {}", paramName, call.getToolName());
            sh.setAttribute("anahata.customClasspath", cp);
        }
        return sh;
    }
}