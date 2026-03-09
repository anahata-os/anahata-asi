/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkits;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import uno.anahata.asi.toolkit.audio.AudioDevice;
import uno.anahata.asi.yam.tools.Radio;

/**
 * A specialized UI for the {@link Radio} toolkit.
 * <p>
 * Features a high-contrast LED display and hardware controller for managing
 * internet radio streams and output routing.
 * </p>
 * 
 * @author anahata
 */
public class RadioRenderer extends AbstractToolkitRenderer<Radio> {

    private JTable stationTable;
    private DefaultTableModel tableModel;
    private JLabel statusLabel;
    private JLabel stationLabel;
    private JButton playBtn;
    private JComboBox<AudioDevice> deviceCombo;

    /**
     * Constructs a new RadioRenderer.
     */
    public RadioRenderer() {
        super();
    }

    /** {@inheritDoc} <p>Initializes the high-contrast radio console and binds listeners.</p> */
    @Override
    protected void onBind() {
        removeAll();
        setLayout(new BorderLayout());
        setBackground(new Color(18, 18, 18));
        setPreferredSize(new Dimension(0, 400));
        
        // NEON BORDER: Purple or something like that as requested
        Color neonPurple = new Color(180, 0, 255);
        setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(neonPurple, 2),
            BorderFactory.createEmptyBorder(10, 10, 10, 10)
        ));

        // --- Left: Station Table ---
        String[] columnNames = {"Station Name", "URL"};
        tableModel = new DefaultTableModel(columnNames, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        Radio.STATIONS.forEach((name, url) -> {
            tableModel.addRow(new Object[]{name, url});
        });

        stationTable = new JTable(tableModel);
        stationTable.setRowHeight(35);
        stationTable.setBackground(new Color(25, 25, 25));
        stationTable.setForeground(new Color(210, 210, 210));
        stationTable.setShowGrid(false);
        stationTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        stationTable.removeColumn(stationTable.getColumnModel().getColumn(1)); // Hide URL

        // Interaction: Single-click to play
        stationTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                int row = stationTable.rowAtPoint(e.getPoint());
                if (row != -1) {
                    stationTable.setRowSelectionInterval(row, row);
                    String url = (String) tableModel.getValueAt(row, 1);
                    anahataToolkit.start(url);
                }
            }
        });

        DefaultTableCellRenderer neonRenderer = new DefaultTableCellRenderer() {
            @Override
            public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                JLabel label = (JLabel) super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                label.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));
                if (isSelected) {
                    label.setBackground(new Color(0, 255, 150, 40));
                    label.setForeground(Color.WHITE);
                    label.setFont(label.getFont().deriveFont(Font.BOLD));
                } else {
                    label.setBackground(table.getBackground());
                    label.setForeground(table.getForeground());
                }
                return label;
            }
        };
        stationTable.setDefaultRenderer(Object.class, neonRenderer);

        JScrollPane tableScroll = new JScrollPane(stationTable);
        tableScroll.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.DARK_GRAY));
        tableScroll.getViewport().setBackground(new Color(25, 25, 25));

        // --- Right: LED & Controls ---
        JPanel rightPanel = new JPanel(new BorderLayout());
        rightPanel.setBackground(new Color(18, 18, 18));

        // LED Display (North)
        JPanel ledPanel = new JPanel(new GridBagLayout());
        ledPanel.setBackground(Color.BLACK);
        ledPanel.setPreferredSize(new Dimension(300, 120));
        ledPanel.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 0, 2, 0, new Color(0, 255, 150)),
            BorderFactory.createEmptyBorder(15, 15, 15, 15)
        ));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0; gbc.gridy = 0; gbc.anchor = GridBagConstraints.WEST;
        statusLabel = new JLabel("● SYSTEM READY");
        statusLabel.setForeground(new Color(0, 180, 255));
        statusLabel.setFont(new Font("Monospaced", Font.BOLD, 12));
        ledPanel.add(statusLabel, gbc);

        gbc.gridy = 1; gbc.weightx = 1.0; gbc.weighty = 1.0; gbc.fill = GridBagConstraints.BOTH;
        stationLabel = new JLabel("ANAHATA RADIO");
        stationLabel.setForeground(new Color(50, 255, 50));
        stationLabel.setFont(new Font("Monospaced", Font.BOLD, 20));
        stationLabel.setHorizontalAlignment(JLabel.CENTER);
        ledPanel.add(stationLabel, gbc);
        
        rightPanel.add(ledPanel, BorderLayout.NORTH);

        // Hardware & Playback Controls (Center)
        JPanel controlsPanel = new JPanel(new GridBagLayout());
        controlsPanel.setBackground(new Color(18, 18, 18));
        
        gbc = new GridBagConstraints();
        gbc.insets = new Insets(15, 10, 15, 10);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.NONE; 
        gbc.anchor = GridBagConstraints.CENTER;

        // PLAY/STOP BUTTON: Middle, above selection as requested
        playBtn = new JButton("▶ PLAY");
        playBtn.setFont(playBtn.getFont().deriveFont(Font.BOLD, 18f));
        playBtn.setPreferredSize(new Dimension(140, 50));
        playBtn.addActionListener(e -> {
            togglePlayback();
        });
        controlsPanel.add(playBtn, gbc);

        // LINE SELECTION: Below play button, no horizontal grow
        gbc.gridy = 1;
        gbc.insets = new Insets(0, 10, 10, 10);
        deviceCombo = new JComboBox<>(new DefaultComboBoxModel<>(AudioDevice.listAvailableDevices(AudioDevice.Type.OUTPUT).toArray(new AudioDevice[0])));
        deviceCombo.setSelectedItem(anahataToolkit.getSelectedOutputDevice());
        deviceCombo.setPreferredSize(new Dimension(220, 25));
        deviceCombo.addActionListener(e -> {
            anahataToolkit.setSelectedOutputDevice((AudioDevice) deviceCombo.getSelectedItem());
        });
        controlsPanel.add(deviceCombo, gbc);

        rightPanel.add(controlsPanel, BorderLayout.CENTER);

        JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tableScroll, rightPanel);
        split.setDividerLocation(250);
        split.setBorder(null);
        add(split, BorderLayout.CENTER);

        syncUiWithToolkit();
    }

    /**
     * Toggles the playback state based on current toolkit status.
     */
    private void togglePlayback() {
        if (anahataToolkit.isPlaying()) {
            anahataToolkit.stop();
        } else {
            int row = stationTable.getSelectedRow();
            if (row != -1) {
                String url = (String) tableModel.getValueAt(row, 1);
                anahataToolkit.start(url);
            }
        }
    }

    /**
     * Synchronizes the UI components with the current state of the Radio toolkit.
     */
    private void syncUiWithToolkit() {
        boolean playing = anahataToolkit.isPlaying();
        playBtn.setText(playing ? "⏹ STOP" : "▶ PLAY");
        statusLabel.setText(playing ? "● STREAMING LIVE" : "● SYSTEM READY");
        statusLabel.setForeground(playing ? new Color(255, 50, 50) : new Color(0, 180, 255));
        
        if (playing && anahataToolkit.getCurrentStationUrl() != null) {
            String name = Radio.STATIONS.entrySet().stream()
                    .filter(e -> e.getValue().equals(anahataToolkit.getCurrentStationUrl()))
                    .map(Map.Entry::getKey)
                    .findFirst()
                    .orElse("Unknown Station");
            stationLabel.setText(name.toUpperCase());
            
            // Highlight in table
            for (int i = 0; i < tableModel.getRowCount(); i++) {
                if (Radio.STATIONS.get(tableModel.getValueAt(i, 0)).equals(anahataToolkit.getCurrentStationUrl())) {
                    stationTable.setRowSelectionInterval(i, i);
                    stationTable.scrollRectToVisible(stationTable.getCellRect(i, 0, true));
                    break;
                }
            }
        } else {
            stationLabel.setText("ANAHATA RADIO");
        }
        
        deviceCombo.setSelectedItem(anahataToolkit.getSelectedOutputDevice());
    }

    /** {@inheritDoc} <p>Refreshes the console display when toolkit properties change.</p> */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        syncUiWithToolkit();
    }
}
