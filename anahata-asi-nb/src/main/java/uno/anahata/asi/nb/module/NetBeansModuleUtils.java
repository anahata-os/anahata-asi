/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.nb.module;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.openide.modules.Dependency;
import org.openide.modules.ModuleInfo;
import org.openide.modules.Modules;
import uno.anahata.asi.nb.AnahataInstaller;

/**
 * Utility class for introspecting NetBeans modules and their classpaths.
 * 
 * @author anahata
 */
public final class NetBeansModuleUtils {

    /** Logger instance for module utility operations. */
    private static final Logger logger = Logger.getLogger(NetBeansModuleUtils.class.getName());
    
    /** Cached classpath string for the NetBeans environment. */
    private static String cachedNetBeansClasspath;

    /**
     * Private constructor to prevent instantiation of this utility class.
     */
    private NetBeansModuleUtils() {
    }

    /**
     * Gets the comprehensive classpath for the NetBeans environment.
     * The result is cached after the first call.
     * 
     * @return The full NetBeans classpath string.
     */
    public static synchronized String getNetBeansClasspath() {
        if (cachedNetBeansClasspath == null) {
            cachedNetBeansClasspath = buildNetBeansClasspath();
        }
        return cachedNetBeansClasspath;
    }

    /**
     * Builds a comprehensive classpath for the NetBeans environment, including
     * module JARs and dynamic paths.
     * 
     * @return The full NetBeans classpath string.
     */
    /**
     * Internal logic to construct the NetBeans classpath by aggregating the 
     * system classpath, dynamic classpath, and all reachable module JARs.
     * 
     * @return The fully assembled classpath string.
     */
    private static String buildNetBeansClasspath() {
        try {
            String javaClassPath = System.getProperty("java.class.path");
            String netbeansDynamicClassPath = System.getProperty("netbeans.dynamic.classpath");
            
            Set<File> moduleClassPath = getModuleClassPath();
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
    private static Set<File> getModuleClassPath() {
        Set<ModuleInfo> processed = new HashSet<>();
        ModuleInfo thisModule = Modules.getDefault().ownerOf(AnahataInstaller.class);
        if (thisModule == null) {
            return Collections.emptySet();
        }
        return getClassPath(thisModule, processed);
    }

    /**
     * Recursively traverses the dependency tree of a module to collect all 
     * associated JAR files.
     * 
     * @param mi The module to start traversal from.
     * @param processed The set of already processed modules to prevent cycles.
     * @return A Set of JAR files for the module and its transitive dependencies.
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
     * @return The ModuleInfo if it's a module dependency and can be found, null otherwise.
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
     * Uses reflection to invoke the non-public {@code getAllJars()} method on 
     * a {@link ModuleInfo} instance. This is necessary to get the full list 
     * of JARs bundled with a module (including library extensions).
     * 
     * @param thisModule The module to inspect.
     * @return A list of JAR files provided by the module.
     */
    public static List<File> getAllModuleJarsUsingReflection(ModuleInfo thisModule) {
        try {
            Method getAllJarsMethod = thisModule.getClass().getMethod("getAllJars");
            getAllJarsMethod.setAccessible(true);
            @SuppressWarnings("unchecked")
            List<File> allJars = (List<File>) getAllJarsMethod.invoke(thisModule);
            return allJars;
        } catch (Exception ex) {
            logger.log(Level.SEVERE, "Exception in getAllModuleJarsUsingReflection for module " + thisModule.getCodeNameBase(), ex);
        }
        return Collections.emptyList();
    }

    /**
     * Converts a set of File objects into a single classpath string using 
     * the platform's path separator.
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
