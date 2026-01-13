/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara! */
package uno.anahata.ai.swing.components;

import com.jgoodies.forms.layout.FormLayout;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeListener;

/**
 * A combined component featuring a JSpinner and a JSlider that stay in sync.
 * It uses JGoodies FormLayout for precise positioning.
 * 
 * @author anahata
 */
public class SliderSpinner extends JPanel {

    /** The underlying spinner. */
    private final JSpinner spinner;
    /** The underlying slider. */
    private final JSlider slider;
    /** The scale factor between the spinner value and the slider value (slider = spinner * scale). */
    private final double scale;
    /** Flag to prevent infinite recursion during sync. */
    private boolean isAdjusting = false;

    /**
     * Constructs a new SliderSpinner.
     * 
     * @param model The SpinnerNumberModel to use.
     * @param sliderMin The minimum value for the slider.
     * @param sliderMax The maximum value for the slider.
     * @param scale The scale factor between the spinner value and the slider value.
     */
    public SliderSpinner(SpinnerNumberModel model, int sliderMin, int sliderMax, double scale) {
        this.spinner = new JSpinner(model);
        this.slider = new JSlider(sliderMin, sliderMax);
        this.scale = scale;

        // Use JGoodies FormLayout: 
        // Column 1: Spinner (fixed width for consistency)
        // Column 2: Gap (4 dialog units)
        // Column 3: Slider (fills remaining space, minimum 150 dialog units)
        setLayout(new FormLayout("50dlu, 4dlu, fill:150dlu:grow", "pref"));
        add(spinner, "1, 1");
        add(slider, "3, 1");

        spinner.addChangeListener(e -> {
            if (!isAdjusting) {
                isAdjusting = true;
                Number val = (Number) spinner.getValue();
                slider.setValue((int) Math.round(val.doubleValue() * scale));
                isAdjusting = false;
            }
        });

        slider.addChangeListener(e -> {
            if (!isAdjusting) {
                isAdjusting = true;
                double val = slider.getValue() / scale;
                if (model.getMinimum() instanceof Double || model.getStepSize() instanceof Double) {
                    spinner.setValue(val);
                } else {
                    spinner.setValue((int) Math.round(val));
                }
                isAdjusting = false;
            }
        });
        
        // Initial sync
        slider.setValue((int) Math.round(((Number)model.getValue()).doubleValue() * scale));
    }

    /**
     * Gets the current value from the spinner.
     * @return The value.
     */
    public Object getValue() {
        return spinner.getValue();
    }

    /**
     * Sets the value of the spinner (and thus the slider).
     * @param value The new value.
     */
    public void setValue(Object value) {
        spinner.setValue(value);
    }

    /**
     * Gets the underlying JSpinner.
     * @return The spinner.
     */
    public JSpinner getSpinner() {
        return spinner;
    }

    /**
     * Gets the underlying JSlider.
     * @return The slider.
     */
    public JSlider getSlider() {
        return slider;
    }
    
    /**
     * Adds a ChangeListener to the underlying spinner.
     * @param listener The listener to add.
     */
    public void addChangeListener(ChangeListener listener) {
        spinner.addChangeListener(listener);
    }

    /** {@inheritDoc} */
    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        spinner.setEnabled(enabled);
        slider.setEnabled(enabled);
    }
}
