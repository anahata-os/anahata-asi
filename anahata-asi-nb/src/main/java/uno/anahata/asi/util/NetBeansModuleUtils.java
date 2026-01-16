/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.util;

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
import uno.anahata.asi.AnahataInstaller;

/**
 * Utility class for introspecting NetBeans modules and their classpaths.
 * 
 * @author anahata
 */
public final class NetBeansModuleUtils {

    private static final Logger logger = Logger.getLogger(NetBeansModuleUtils.class.getName());
    
    private static String cachedNetBeansClasspath;

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

    private static Set<File> getModuleClassPath() {
        Set<ModuleInfo> processed = new HashSet<>();
        ModuleInfo thisModule = Modules.getDefault().ownerOf(AnahataInstaller.class);
        if (thisModule == null) {
            return Collections.emptySet();
        }
        return getClassPath(thisModule, processed);
    }

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

    private static ModuleInfo getDependantModuleInfo(Dependency d) {
        Modules modules = Modules.getDefault();
        if (d.getType() == Dependency.TYPE_MODULE) {
            String codeName = d.getName();
            String codeNameBase = codeName.contains("/") ? codeName.substring(0, codeName.indexOf('/')) : codeName;
            return modules.findCodeNameBase(codeNameBase);
        }
        return null;
    }

    private static List<File> getAllModuleJarsUsingReflection(ModuleInfo thisModule) {
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
