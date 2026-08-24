/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# NetBeans NBM Hot Reloading Architecture (`nbmreload`)

This document consolidates the complete architectural findings, lifecycle patterns, memory management contracts, and NetBeans platform internal types discovered and hardened for the **`anahata-asi-nb`** module.

---

## 1. High-Level Lifecycle Flow

When an `nbmreload` action is executed (via `nbactions.xml` or NetBeans project actions):

```text
[1. Trigger] Maven builds and deploys uno-anahata-asi-nb.jar to clusters/extra/modules/
      │
[2. Uninstall Phase] NetBeans ModuleManager invokes AnahataInstaller.uninstalled() on EDT
      │
      ├── A. Set 'anahata.nbmreload.pending=true' system property
      ├── B. Detach all open TopComponents via ReloadableTopComponent.detachForNbmReload()
      │        ├── AgiTopComponent: nulls agiPanel, disconnects session, closes tab
      │        └── AbstractAsiContainerTopComponent: stops javax.swing.Timer, un-docks from ModeImpl, closes tab
      ├── C. Invalidate cached .settings DataObjects in Windows2Local/Components
      │        └── DataObject.setValid(false) evicts SoftReferences from DataObjectPool
      ├── D. Clear 'AGI' from PersistenceManager.globalIDSet (prevents _1, _2 suffix bloat)
      └── E. NetBeansAsiContainer.shutdown() terminates background executor thread pools
      │
[3. ClassLoader Tear-Down]
      ├── StandardModule.classLoaderDown()
      ├── StandardModule.releaseClassLoader()
      ├── System.gc() + System.runFinalization() sweeps old OneModuleClassLoader
      └── StandardModule.cleanup()
      │
[4. Install & Restore Phase]
      ├── NetBeans constructs new OneModuleClassLoader (ClassLoader v2)
      ├── ModuleManager invokes AnahataInstaller.restored() on Main/EDT
      ├── AnahataInstaller.loadSessions() restores session state from ~/.anahata/asi/netbeans/sessions/
      └── Reopens all active AGI tabs on ClassLoader v2 automatically!
```

---

## 2. Memory Management & Reference Topology

A successful, leak-free reload requires that **all Strong references** and **`SoftReference` caches** pointing to types on the old `OneModuleClassLoader` are severed before `System.gc()` runs.

### A. Strong (Hard) References (Must Be Explicitly Broken)
1. **`javax.swing.Timer` in `AbstractAsiContainerPanel`**:
   - The shared Swing `TimerQueue` holds a strong reference to any running timer.
   - **Fix**: `componentClosed()` and `detachForNbmReload()` call `sessionsPanel.stopRefresh()` to stop the timer.
2. **`Model` / `TopComponentSubModel.openedTopComponents` (`List<TopComponent>`)**:
   - NetBeans' internal window model holds strong references to open tabs.
   - **Fix**: `modeImpl.removeTopComponent(this)` and `close()` remove the component from `openedTopComponents`.
3. **`WindowManager.Registry` (`RegistryImpl.opened`)**:
   - Global set of open windows in the IDE.
   - **Fix**: `tc.close()` unregisters the component from the registry.
4. **Executor Service Threads (`NetBeansAsiContainer.executor`)**:
   - Active worker threads hold references to the container and module classloader.
   - **Fix**: `container.shutdown()` cleanly terminates the thread pool.

### B. Soft References (`DataObjectPool` & `SettingsInstance`)
- When NetBeans loads a `.settings` file (`Windows2Local/Components/*.settings`), `InstanceDataObject` caches the created instance in `SerialDataConvertor$SettingsInstance.inst` as a **`java.lang.ref.SoftReference`**.
- Standard `System.gc()` does **NOT** collect `SoftReference`s unless JVM heap memory is exhausted.
- **Fix**: Calling `DataObject.find(fo).setValid(false)` in `invalidateWindows2LocalComponentDataObjects()` forces `DataObjectPool` to discard the `InstanceDataObject` and its `SoftReference`, firing `DataObject.PROP_COOKIE` to purge `PersistenceManager`'s internal maps.

### C. Weak References (`PersistenceManager` & `TopComponentTracker`)
- `PersistenceManager.id2TopComponentMap`: `Map<String, WeakReference<TopComponent>>`
- `PersistenceManager.topComponent2IDMap`: `WeakHashMap<TopComponent, String>`
- `TopComponentTracker.editors`: `WeakHashMap<TopComponent, Boolean>`
- These references **do not prevent GC**. Once strong and soft references are severed, the JVM sweeps these entries automatically.

---

## 3. TopComponent & Mode Mechanics

### A. Avoid Lazy-Loading via `mode.getTopComponents()`
- In `TopComponentSubModel.java`:
  - `openedTopComponents` (`List<TopComponent>`): Stores **only live, open instances**.
  - `tcIDs` (`List<String>`): Stores **String IDs for both opened and closed/unloaded components** (including prototype templates like `"agi"`).
- Calling `mode.getTopComponents()` forces NetBeans to iterate `tcIDs` and deserialize every closed component via `PersistenceManager.getTopComponentForID(id, true)`.
- **Golden Rule**: Always iterate over **`WindowManager.getDefault().getRegistry().getOpened()`** or **`modeImpl.getOpenedTopComponents()`** to avoid accidentally creating dormant prototype instances!

### B. The `ReloadableTopComponent` Contract
Every Anahata `TopComponent` implements `uno.anahata.asi.nb.ReloadableTopComponent`:
```java
public interface ReloadableTopComponent {
    void detachForNbmReload();
}
```
- **`AgiTopComponent`**: Nulls `agiPanel`, resets `sessionId`, and calls `close()` without syncing `open=false` to the container (so the session remains logically open for `restored()`).
- **`AbstractAsiContainerTopComponent`** (`AsiCardsTopComponent`, `AsiTableTopComponent`): Stops the `Timer`, un-docks from `ModeImpl`, and calls `close()`.

---

## 4. Action & Layer Architecture (`NewAgiAction`)

### A. NetBeans `ActionProcessor` & Localization Bundles
- `@ActionRegistration` is processed by `org.openide.awt.ActionProcessor`.
- When `displayName = "#CTL_NewAgiAction"` is used, `ActionProcessor` writes `<attr bundlevalue="uno.anahata.asi.nb.Bundle#CTL_NewAgiAction" name="displayName"/>` into `META-INF/generated-layer.xml`.
- NetBeans resolves this from `src/main/resources/uno/anahata/asi/nb/Bundle.properties`.
- If the physical `Bundle.properties` file in `src/main/resources` is missing the key, `displayName` resolves to `null`, causing the menu item to render only the icon.
- **Rule**: All `@ActionRegistration` bundle keys must exist in `src/main/resources/uno/anahata/asi/nb/Bundle.properties`.

### B. Action Shadow Links (`.shadow`)
- NetBeans registers actions once in `Actions/<Category>/<ActionName>.instance`.
- Menu items and toolbars are created via symbolic pointer files: `Menu/Window/<ActionName>.shadow`.
- The `.shadow` file contains an `originalFile` attribute pointing to `Actions/Window/<ActionName>.instance`.

---

## 5. NetBeans Internal Types & Interfaces Reference (FQNs)

The following core NetBeans Platform types were analyzed and verified during this architectural hardening:

### Window System & Persistence APIs
| Canonical FQN | Role in NetBeans Platform |
| :--- | :--- |
| `org.openide.windows.TopComponent` | Base UI component for all window system tabs and frames. |
| `org.openide.windows.WindowManager` | Public singleton manager for window operations, modes, and registries. |
| `org.openide.windows.Mode` | Public interface representing visual docking containers (`editor`, `output`, `explorer`). |
| `org.openide.windows.TopComponent$Registry` | Tracks currently opened, activated, and focused TopComponents. |
| `org.netbeans.core.windows.WindowManagerImpl` | Core singleton implementation of `WindowManager` and `Workspace`. |
| `org.netbeans.core.windows.Central` | Central mediator connecting Model, View, and Controller handlers. |
| `org.netbeans.core.windows.ModeImpl` | Core implementation of `Mode` and `Mode.Xml`. |
| `org.netbeans.core.windows.TopComponentTracker` | Persistent tracker distinguishing editor document windows from tool views. |
| `org.netbeans.core.windows.model.Model` | Central interface for the in-memory window system data model. |
| `org.netbeans.core.windows.model.DefaultModel` | Concrete implementation of `Model` managing mode structures and snapshots. |
| `org.netbeans.core.windows.model.DefaultModeModel` | Per-mode model managing state, bounds, and component lists. |
| `org.netbeans.core.windows.model.ModesSubModel` | Manages split constraints, sliding modes, and docking trees. |
| `org.netbeans.core.windows.model.TopComponentSubModel` | Stores `openedTopComponents` (objects) and `tcIDs` (string IDs). |
| `org.netbeans.core.windows.persistence.PersistenceManager` | Core persistence engine managing `.settings` loading, saving, and ID mapping. |
| `org.netbeans.core.windows.persistence.PersistenceHandler` | Handles window layout persistence events and snapshot serialization. |

### Module System & ClassLoading APIs
| Canonical FQN | Role in NetBeans Platform |
| :--- | :--- |
| `org.openide.modules.ModuleInstall` | Base lifecycle class (`restored()`, `uninstalled()`, `close()`). |
| `org.openide.modules.ModuleInfo` | Metadata provider for an installed module (`codeNameBase`, `specVersion`, `dependencies`). |
| `org.openide.modules.Modules` | Singleton registry of all available modules in the IDE runtime. |
| `org.netbeans.ModuleManager` | Core engine managing module dependencies, enablement, disabling, and topological sorting. |
| `org.netbeans.StandardModule` | Represents standard NBM modules loaded from physical JAR files. |
| `org.netbeans.StandardModule$OneModuleClassLoader` | The dedicated `JarClassLoader` instance created for each NetBeans module. |
| `org.netbeans.JarClassLoader` | High-performance multi-release JAR classloader with covered-package routing. |
| `org.netbeans.ProxyClassLoader` | Multi-parented classloader routing class/resource lookups across module dependencies. |
| `org.netbeans.core.startup.ModuleSystem` | Top-level startup controller managing bootstrap, module list, and shutdown sequences. |
| `org.netbeans.core.startup.NbInstaller` | NetBeans-specific installer managing layers, `ModuleInstall` classes, and section loading. |
| `org.openide.loaders.InstanceDataObject` | `DataObject` wrapper for `.settings` files that instantiates and caches TopComponents. |
| `org.openide.loaders.DataObjectPool` | Global IDE singleton cache holding all active `DataObject` instances. |
| `org.openide.awt.ActionProcessor` | Annotation processor compiling `@ActionID`, `@ActionRegistration`, and `@ActionReference`. |

---

*Força Barça!*
