package uno.anahata.ai.tool;

import java.util.Map;
import java.util.concurrent.Callable;
import uno.anahata.ai.toolkit.Java;

/**
 * Base class for model generated java.
 * 
 * @author anahata
 */
public abstract class AnahataTool extends HandyToolStuff implements Callable<Object>{
    
    /**
     * A map for the model to store objects across turns.
     */
    protected Map sessionMap;

    
    public Map getSessionMap() {
        return sessionMap;
    }
    
}
