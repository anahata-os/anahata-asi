package uno.anahata.ai.tool;

import java.util.Map;
import java.util.concurrent.Callable;
import uno.anahata.ai.model.tool.java.JavaObjectToolkit;
import uno.anahata.ai.toolkit.Java;

/**
 * Base class for model generated java.
 * 
 * @author anahata
 */
public abstract class AnahataTool extends HandyToolStuff implements Callable<Object>{
    
    public Map getSessionMap() {
        JavaObjectToolkit jot = (JavaObjectToolkit) getTool().getToolkit();
        Java tool = (Java) jot.getToolInstance();
        return tool.sessionMap;
    }
    
    public Map getApplicaitonMap() {
        JavaObjectToolkit jot = (JavaObjectToolkit) getTool().getToolkit();
        Java tool = (Java) jot.getToolInstance();
        return tool.applicationMap;
    }
    
    /**
     * For the model to be able to set the "turns to keep" property for this call on the same turn 
     * 
     * @param turnsToKeep 
     */
    public void setTurnsToKeep(int turnsToKeep) {
        getResponse().setTurnsToKeep(turnsToKeep);
        getCall().setTurnsToKeep(turnsToKeep);
    }
    
    
    
}
