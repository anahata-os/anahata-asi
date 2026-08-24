/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.nb.module;

import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.api.autoupdate.OperationContainer;
import org.netbeans.api.autoupdate.OperationSupport;
import org.netbeans.api.autoupdate.UpdateElement;
import org.netbeans.api.autoupdate.UpdateManager;
import org.netbeans.api.autoupdate.UpdateUnit;
import org.openide.modules.Dependency;
import org.openide.modules.ModuleInfo;
import org.openide.modules.Modules;
import org.openide.util.Lookup;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.nb.AnahataInstaller;

/**
 * Utility class for introspecting NetBeans modules and their classpaths.
 *
 * @author anahata
 */
public final class NetBeansModuleUtils {

    /**
     * Logger instance for module utility operations.
     */
    private static final Logger logger = Logger.getLogger(NetBeansModuleUtils.class.getName());

    /**
     * Cached classpath string for the NetBeans environment.
     */
    private static String cachedAnahataAsiPluginClasspath;

    /**
     * Private constructor to prevent instantiation of this utility class.
     */
    private NetBeansModuleUtils() {
    }

    /**
     * Gets the comprehensive classpath for the NetBeans environment. The result
     * is cached after the first call.
     *
     * @return The full NetBeans classpath string.
     */
    public static synchronized String getFullAnahataAsiModuleClasspath() {
        if (cachedAnahataAsiPluginClasspath == null) {
            cachedAnahataAsiPluginClasspath = buildFullAnahataAsiModuleClasspath();
        }
        return cachedAnahataAsiPluginClasspath;
    }

    /**
     * Internal logic to construct the NetBeans classpath by aggregating the
     * system classpath, dynamic classpath, and all reachable module JARs.
     *
     * @return The fully assembled classpath string.
     */
    private static String buildFullAnahataAsiModuleClasspath() {
        try {
            String javaClassPath = System.getProperty("java.class.path");
            String netbeansDynamicClassPath = System.getProperty("netbeans.dynamic.classpath");

            Set<File> moduleClassPath = getAnahataAsiModuleClassPath();
            String moduleClassPathStr = filesToClassPathString(moduleClassPath);

            StringBuilder sb = new StringBuilder();
            sb.append(javaClassPath);
            if (netbeansDynamicClassPath != null && !netbeansDynamicClassPath.isEmpty()) {
                sb.append(File.pathSeparator).append(netbeansDynamicClassPath);
            }
            if (!moduleClassPathStr.isEmpty()) {
                sb.append(File.pathSeparator).append(moduleClassPathStr);
            }

            return sb.toString();
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Exception building NetBeans classpath", e);
            return System.getProperty("java.class.path");
        }
    }

    /**
     * Retrieves the set of JAR files that constitute the classpath of the
     * current Anahata module and all its dependencies.
     *
     * @return A Set of File objects representing the module classpath.
     */
    private static Set<File> getAnahataAsiModuleClassPath() {
        Set<ModuleInfo> processed = new HashSet<>();
        ModuleInfo thisModule = Modules.getDefault().ownerOf(AnahataInstaller.class);
        if (thisModule == null) {
            return Collections.emptySet();
        }
        return getClassPath(thisModule, processed);
    }

    /**
     * Checks whether a JavaFX runtime provider module is installed in NetBeans (enabled or disabled).
     *
     * @return {@code true} if a module providing the JavaFX token is installed, {@code false} otherwise.
     */
    public static boolean isJavaFxModuleInstalled() {
        for (ModuleInfo mi : Lookup.getDefault().lookupAll(ModuleInfo.class)) {
            for (String token : mi.getProvides()) {
                if ("org.openide.modules.jre.JavaFX".equals(token)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks whether a JavaFX runtime provider module is currently enabled and active in NetBeans.
     *
     * @return {@code true} if active, {@code false} otherwise.
     */
    public static boolean isJavaFxModuleEnabled() {
        return getEnabledJavaFxModule() != null;
    }

    /**
     * Finds the currently enabled JavaFX provider module in NetBeans
     * (cross-platform).
     *
     * @return The active ModuleInfo providing org.openide.modules.jre.JavaFX,
     * or null if absent.
     */
    public static ModuleInfo getEnabledJavaFxModule() {
        for (ModuleInfo mi : Lookup.getDefault().lookupAll(ModuleInfo.class)) {
            if (!mi.isEnabled()) {
                continue;
            }
            for (String token : mi.getProvides()) {
                if ("org.openide.modules.jre.JavaFX".equals(token)) {
                    return mi;
                }
            }
        }
        return null;
    }

    /**
     * Gets the ClassLoader of the enabled JavaFX provider module in NetBeans,
     * or null if absent.
     *
     * @return The JavaFX module ClassLoader, or null.
     */
    public static ClassLoader getJavaFxModuleClassLoader() {
        ModuleInfo fxModule = getEnabledJavaFxModule();
        return fxModule != null ? fxModule.getClassLoader() : null;
    }

    /**
     * Gets the classpath string for the enabled JavaFX provider module's JARs.
     *
     * @return The classpath string of JavaFX JARs, or null if JavaFX is not
     * enabled in NetBeans.
     */
    public static String getJavaFxModuleClasspath() {
        ModuleInfo fxModule = getEnabledJavaFxModule();
        if (fxModule == null) {
            return null;
        }
        List<File> jars = getAllModuleJarsUsingReflection(fxModule);
        return filesToClassPathString(new HashSet<>(jars));
    }

    /**
     * Gets the active JavaFX runtime version, or null if absent.
     *
     * @return The version string, or null.
     */
    public static String getJavaFxVersion() {
        try {
            Class<?> versionInfo = Class.forName("com.sun.javafx.runtime.VersionInfo");
            return (String) versionInfo.getMethod("getVersion").invoke(null) + " (System JDK)";
        } catch (Throwable ignored) {
        }

        ModuleInfo fxModule = getEnabledJavaFxModule();
        if (fxModule != null && fxModule.getClassLoader() != null) {
            try {
                Class<?> versionInfo = fxModule.getClassLoader().loadClass("com.sun.javafx.runtime.VersionInfo");
                return (String) versionInfo.getMethod("getVersion").invoke(null) + " (" + fxModule.getCodeNameBase() + ")";
            } catch (Throwable ignored) {
            }
        }
        return null;
    }
    
    /**
     * Installs and activates the NetBeans JavaFX runtime support programmatically.
     *
     * @return The installation result status.
     * @throws Exception if installation fails.
     */
    @AgiTool("Installs and activates the NetBeans JavaFX runtime support programmatically on demand.")
    public static String installJavaFxSupport() throws Exception {
        
        UpdateUnit unit = UpdateManager.getDefault().getUpdateUnits(UpdateManager.TYPE.MODULE)
                .stream()
                .filter(u -> "org.netbeans.modules.javafx2.kit".equals(u.getCodeName()))
                .findFirst()
                .orElseThrow(() -> new AgiToolException("JavaFX 2 Support module (org.netbeans.modules.javafx2.kit) not found in NetBeans."));

        UpdateElement installed = unit.getInstalled();
        if (installed == null) {
            throw new AgiToolException("JavaFX 2 Support is not installed on disk.");
        }
        if (installed.isEnabled()) {
            return "JavaFX support is already active. Version: " + NetBeansModuleUtils.getJavaFxVersion();
        }

        OperationContainer<OperationSupport> container = OperationContainer.createForEnable();
        OperationContainer.OperationInfo<OperationSupport> info = container.add(installed);
        if (info != null) {
            container.add(info.getRequiredElements());
            OperationSupport support = container.getSupport();
            support.doOperation(null);
            return "JavaFX support successfully activated! Version: " + NetBeansModuleUtils.getJavaFxVersion();
        }
        return "Unable to enable JavaFX support.";
    }

    /**
     * Recursively traverses the dependency tree of a module to collect all
     * associated JAR files.
     *
     * @param mi The module to start traversal from.
     * @param processed The set of already processed modules to prevent cycles.
     * @return A Set of JAR files for the module and its transitive
     * dependencies.
     */
    private static Set<File> getClassPath(ModuleInfo mi, Set<ModuleInfo> processed) {
        Set<File> ret = new HashSet<>();
        processed.add(mi);
        ret.addAll(getAllModuleJarsUsingReflection(mi));
        for (Dependency d : mi.getDependencies()) {
            ModuleInfo dependantModule = getDependantModuleInfo(d);
            if (dependantModule != null && !processed.contains(dependantModule)) {
                ret.addAll(getClassPath(dependantModule, processed));
            }
        }
        return ret;
    }

    /**
     * Resolves a dependency to its corresponding {@link ModuleInfo}.
     *
     * @param d The dependency to resolve.
     * @return The ModuleInfo if it's a module dependency and can be found, null
     * otherwise.
     */
    private static ModuleInfo getDependantModuleInfo(Dependency d) {
        Modules modules = Modules.getDefault();
        if (d.getType() == Dependency.TYPE_MODULE) {
            String codeName = d.getName();
            String codeNameBase = codeName.contains("/") ? codeName.substring(0, codeName.indexOf('/')) : codeName;
            return modules.findCodeNameBase(codeNameBase);
        }
        return null;
    }

    /**
     * Uses reflection to invoke {@code getJarFile()} and {@code getAllJars()}
     * on a {@link ModuleInfo} instance (such as NetBeans'
     * {@code StandardModule}). Guarantees that the primary module JAR is always
     * included while discarding duplicates.
     *
     * @param thisModule The module to inspect.
     * @return A list of unique JAR files provided by the module.
     */
    public static List<File> getAllModuleJarsUsingReflection(ModuleInfo thisModule) {
        List<File> result = new ArrayList<>();

        try {
            Method getJarFileMethod = thisModule.getClass().getMethod("getJarFile");
            getJarFileMethod.setAccessible(true);
            Object mainJar = getJarFileMethod.invoke(thisModule);
            if (mainJar instanceof File f && f.exists()) {
                result.add(f);
            }
        } catch (Exception ex) {
            logger.log(Level.SEVERE, "Exception invoking getJarFile via reflection on module " + thisModule.getCodeNameBase(), ex);
        }

        try {
            Method getAllJarsMethod = thisModule.getClass().getMethod("getAllJars");
            getAllJarsMethod.setAccessible(true);
            @SuppressWarnings("unchecked")
            List<File> allJars = (List<File>) getAllJarsMethod.invoke(thisModule);
            if (allJars != null) {
                for (File f : allJars) {
                    if (f != null && f.exists() && !result.contains(f)) {
                        result.add(f);
                    }
                }
            }
        } catch (Exception ex) {
            logger.log(Level.SEVERE, "Exception in getAllModuleJarsUsingReflection for module " + thisModule.getCodeNameBase(), ex);
        }
        return result;
    }

    /**
     * Converts a set of File objects into a single classpath string using the
     * platform's path separator.
     *
     * @param classPath The set of files to process.
     * @return A formatted classpath string.
     */
    private static String filesToClassPathString(Set<File> classPath) {
        StringBuilder sb = new StringBuilder();
        for (File jarFile : classPath) {
            if (sb.length() > 0) {
                sb.append(File.pathSeparator);
            }
            sb.append(jarFile.getAbsolutePath());
        }
        return sb.toString();
    }
}
