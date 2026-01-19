/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.standalone.swing;

import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.cli.CommandLineArgs;

/**
 *
 * @author anahata
 */
public class StandaloneAsiContainer extends AsiContainer{
    private String[] cmdLineArgs;
    
    public StandaloneAsiContainer(String[] cmdLineArgs) {
        super("swing-standalone");
        this.cmdLineArgs = cmdLineArgs;
    }

    @Override
    public void onChatCreated(Chat chat) {
        super.onChatCreated(chat); 
        CommandLineArgs.parse(chat, cmdLineArgs);
    }
    
    
}
