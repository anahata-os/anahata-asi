/* Shared Navigation Component for Anahata ASI Web */
document.addEventListener('DOMContentLoaded', () => {
    const navPlaceholder = document.getElementById('main-nav');
    if (!navPlaceholder)
        return;

    const currentPath = window.location.pathname;
    const isIndex = currentPath.endsWith('index.html') || currentPath.endsWith('/');

    // Detection logic
    const activeFile = currentPath.split('/').pop();
    const isActive = (file) => activeFile === file;

    // Compute directory depth prefix dynamically based on the script location
    const navScript = document.querySelector('script[src*="nav.js"]');
    const scriptAttr = navScript ? navScript.getAttribute('src') : 'nav.js';
    const prefix = scriptAttr.includes('nav.js') ? scriptAttr.substring(0, scriptAttr.indexOf('nav.js')) : '';

    navPlaceholder.innerHTML = `
        <nav class="${isIndex ? '' : 'nav-solid'}">
            <div class="nav-container container">
                <a href="${prefix}index.html" class="logo">
                    <img src="${prefix}assets/logo-transparent.png" alt="Anahata Logo" style="height: 40px;">
                </a>
                <div class="menu-toggle"><i class="fas fa-bars"></i></div>
                <div class="nav-links">
                    
                    <!-- Docs Dropdown -->
                    <div class="dropdown">
                        <a href="#" class="dropdown-toggle ${(isActive('quickstart.html') || currentPath.includes('apidocs') || isActive('core.html') || isActive('swing.html') || isActive('yam.html') || isActive('gemini.html') || isActive('openai.html') || isActive('anthropic.html') || isActive('compatible.html')) ? 'active-link' : ''}">
                            Docs <i class="fas fa-chevron-down"></i>
                        </a>
                        <div class="dropdown-menu">
                            <div class="dropdown-header" style="padding: 6px 12px; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 1px; color: var(--barca-gold); font-weight: 800;">Developer Docs</div>
                            <a href="${prefix}quickstart.html" class="${isActive('quickstart.html') ? 'active-item' : ''}"><i class="fas fa-bolt"></i> Quick Start</a>
                            <a href="${prefix}apidocs/index.html"><i class="fas fa-book"></i> Platform Javadocs</a>
                            
                            <div style="border-top: 1px solid rgba(255,255,255,0.1); margin: 6px 0;"></div>
                            <div class="dropdown-header" style="padding: 6px 12px; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 1px; color: var(--barca-gold); font-weight: 800;">Modules</div>
                            <a href="${prefix}core.html" class="${isActive('core.html') ? 'active-item' : ''}"><i class="fas fa-brain"></i> Core API</a>
                            <a href="${prefix}swing.html" class="${isActive('swing.html') ? 'active-item' : ''}"><i class="fas fa-desktop"></i> Swing UI</a>
                            <a href="${prefix}yam.html" class="${isActive('yam.html') ? 'active-item' : ''}"><i class="fas fa-flask"></i> Yam Tools</a>
                            <a href="${prefix}gemini.html" class="${isActive('gemini.html') ? 'active-item' : ''}"><i class="fas fa-plug"></i> Gemini Provider</a>
                            <a href="${prefix}openai.html" class="${isActive('openai.html') ? 'active-item' : ''}"><i class="fas fa-bolt"></i> OpenAI Provider</a>
                            <a href="${prefix}anthropic.html" class="${isActive('anthropic.html') ? 'active-item' : ''}"><i class="fas fa-ghost"></i> Anthropic Provider</a>
                            <a href="${prefix}compatible.html" class="${isActive('compatible.html') ? 'active-item' : ''}"><i class="fas fa-globe"></i> Universal Alliance</a>
                        </div>
                    </div>

                    <!-- Applications Dropdown -->
                    <div class="dropdown">
                        <a href="#" class="dropdown-toggle ${(isActive('nb.html') || isActive('desktop.html') || isActive('intellij.html')) ? 'active-link' : ''}">
                            Applications <i class="fas fa-chevron-down"></i>
                        </a>
                        <div class="dropdown-menu">
                            <a href="${prefix}nb.html" class="${isActive('nb.html') ? 'active-item' : ''}"><i class="fas fa-code"></i> NetBeans ASI Studio</a>
                            <a href="${prefix}desktop.html" class="${isActive('desktop.html') ? 'active-item' : ''}"><i class="fas fa-rocket"></i> Anahata ASI Desktop</a>
                            <a href="${prefix}intellij.html" class="${isActive('intellij.html') ? 'active-item' : ''}"><i class="fas fa-laptop-code"></i> IntelliJ ASI Studio <span class="badge" style="background: var(--accent); color: white; font-size: 0.65rem; padding: 1px 6px; border-radius: 4px; font-weight: 800; margin-left: 4px;">BETA</span></a>
                        </div>
                    </div>

                    <!-- Enterprise Dropdown -->
                    <div class="dropdown">
                        <a href="#" class="dropdown-toggle ${(isActive('enterprise.html') || isActive('defense.html') || isActive('finance.html') || isActive('healthcare.html') || isActive('public-sector.html') || isActive('legal.html') || isActive('logistics.html') || isActive('telecom.html')) ? 'active-link' : ''}">
                            Enterprise <i class="fas fa-chevron-down"></i>
                        </a>
                        <div class="dropdown-menu">
                            <a href="${prefix}enterprise.html" class="${isActive('enterprise.html') ? 'active-item' : ''}"><i class="fas fa-shield-halved"></i> Security Overview</a>
                            <div style="border-top: 1px solid rgba(255,255,255,0.1); margin: 5px 0;"></div>
                            <a href="${prefix}enterprise/defense.html" class="${isActive('defense.html') ? 'active-item' : ''}"><i class="fas fa-shield-alt"></i> Defense & Intel</a>
                            <a href="${prefix}enterprise/finance.html" class="${isActive('finance.html') ? 'active-item' : ''}"><i class="fas fa-landmark"></i> Finance & Banking</a>
                            <a href="${prefix}enterprise/healthcare.html" class="${isActive('healthcare.html') ? 'active-item' : ''}"><i class="fas fa-dna"></i> Healthcare & Pharma</a>
                            <a href="${prefix}enterprise/public-sector.html" class="${isActive('public-sector.html') ? 'active-item' : ''}"><i class="fas fa-gavel"></i> Public Sector</a>
                            <a href="${prefix}enterprise/legal.html" class="${isActive('legal.html') ? 'active-item' : ''}"><i class="fas fa-scale-balanced"></i> Legal & Ethics</a>
                            <a href="${prefix}enterprise/logistics.html" class="${isActive('logistics.html') ? 'active-item' : ''}"><i class="fas fa-truck-ramp-box"></i> Logistics & Supply</a>
                            <a href="${prefix}enterprise/telecom.html" class="${isActive('telecom.html') ? 'active-item' : ''}"><i class="fas fa-wifi"></i> Telecom & 6G</a>
                            <a href="${prefix}enterprise/energy.html" class="${isActive('energy.html') ? 'active-item' : ''}"><i class="fas fa-bolt"></i> Energy & Utilities</a>
                        </div>
                    </div>

                    <!-- Benchmarks Direct Link -->
                    <a href="${prefix}benchmarks.html" class="${(isActive('benchmarks.html') || currentPath.includes('/benchmarks/')) ? 'active-link' : ''}" style="display: flex; align-items: center; gap: 6px; color: var(--white); text-decoration: none; font-weight: 600; font-size: 0.95rem; padding: 0.5rem 0.8rem; border-radius: 6px; transition: var(--transition);">
                        <i class="fas fa-trophy" style="color: var(--barca-gold);"></i> Benchmarks
                    </a>
                    
                    <div class="social-links">
                        <a href="https://www.youtube.com/@anahata108" target="_blank" title="Anahata TV"><i class="fab fa-youtube"></i></a>
                        <a href="https://discord.gg/gwGWWxPUXE" target="_blank" title="Discord"><i class="fab fa-discord"></i></a>
                        <a href="https://x.com/AnahataASI" target="_blank" title="Twitter / X"><i class="fa-brands fa-x-twitter"></i></a>
                        <a href="https://github.com/anahata-os/anahata-asi" target="_blank" title="GitHub"><i class="fab fa-github"></i></a>
                    </div>
                    <a href="https://www.paypal.com/donate/?hosted_button_id=SS8B8R7S68R7G" target="_blank" class="btn-sponsor" style="background: var(--barca-red); color: white; border: none;">Donate</a>
                </div>
            </div>
        </nav>
    `;

    // Mobile Toggle Logic
    const toggle = document.querySelector('.menu-toggle');
    const links = document.querySelector('.nav-links');
    if (toggle && links) {
        toggle.addEventListener('click', () => {
            links.classList.toggle('active');
        });
    }

    // ScrollSpy Logic for Documentation Sidebars
    const initScrollSpy = () => {
        const sidebarLinks = document.querySelectorAll('.sidebar-nav a');
        const sections = Array.from(sidebarLinks).map(link => {
            const href = link.getAttribute('href');
            return href.startsWith('#') ? document.querySelector(href) : null;
        }).filter(s => s !== null);

        if (sidebarLinks.length === 0 || sections.length === 0)
            return;

        const observerOptions = {
            root: null,
            rootMargin: '-150px 0px -70% 0px', // Focus on the top part of the viewport
            threshold: 0
        };

        const observer = new IntersectionObserver((entries) => {
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    const id = entry.target.getAttribute('id');
                    sidebarLinks.forEach(link => {
                        link.classList.toggle('active', link.getAttribute('href') === `#${id}`);
                    });
                }
            });
        }, observerOptions);

        sections.forEach(section => observer.observe(section));

        // Fallback for manual clicks
        sidebarLinks.forEach(link => {
            link.addEventListener('click', () => {
                setTimeout(() => {
                    sidebarLinks.forEach(l => l.classList.remove('active'));
                    link.classList.add('active');
                }, 100);
            });
        });
    };

    // Carousel Logic for desktop.html
    if (isActive('desktop.html') || isActive('nb.html')) {
        let slideIndex = 1;

        window.plusSlides = (n) => {
            showSlides(slideIndex += n);
        };

        window.currentSlide = (n) => {
            showSlides(slideIndex = n);
        };

        function showSlides(n) {
            let i;
            let slides = document.getElementsByClassName("carousel-slide");
            if (slides.length === 0)
                return;
            if (n > slides.length) {
                slideIndex = 1
            }
            if (n < 1) {
                slideIndex = slides.length
            }
            for (i = 0; i < slides.length; i++) {
                slides[i].style.display = "none";
            }
            slides[slideIndex - 1].style.display = "block";
        }

        showSlides(slideIndex);
        // Auto play
        setInterval(() => {
            plusSlides(1);
        }, 5000);
    }


    // Media Lightbox Logic
    const modalHtml = `
        <div id="media-modal" class="modal">
            <div id="modal-media-container"></div>
            <div class="modal-close"><i class="fas fa-times"></i></div>
            <div class="modal-caption" id="modal-caption"></div>
        </div>
    `;
    document.body.insertAdjacentHTML('beforeend', modalHtml);

    const modal = document.getElementById('media-modal');
    const container = document.getElementById('modal-media-container');
    const caption = document.getElementById('modal-caption');

    document.querySelectorAll('.clickable-media').forEach(media => {
        media.addEventListener('click', () => {
            container.innerHTML = '';
            const isVideo = media.tagName.toLowerCase() === 'video';
            const clone = media.cloneNode(true);

            clone.classList.remove('clickable-media');
            clone.classList.add('modal-content');
            clone.removeAttribute('style');

            if (isVideo) {
                clone.controls = true;
                clone.autoplay = true;
                clone.muted = false; // Unmute for full-screen experience
            }

            container.appendChild(clone);
            caption.textContent = media.getAttribute('data-caption') || '';
            modal.classList.add('active');
            document.body.style.overflow = 'hidden';
        });
    });

    modal.addEventListener('click', () => {
        modal.classList.remove('active');
        container.innerHTML = '';
        document.body.style.overflow = '';
    });

    // Dynamic Releases Asset Resolver
    const initDynamicDownloads = async () => {
        // Desktop Snapshot elements
        const winSnapBtn = document.getElementById('dl-windows');
        const macSnapBtn = document.getElementById('dl-macos');
        const linSnapBtn = document.getElementById('dl-linux');
        const deskSnapshotVer = document.getElementById('desktop-snapshot-ver');

        // Desktop Stable elements
        const winStableBtn = document.getElementById('dl-win-stable');
        const macStableBtn = document.getElementById('dl-mac-stable');
        const linStableBtn = document.getElementById('dl-lin-stable');
        const deskStableVer = document.getElementById('desktop-stable-ver');

        // NetBeans elements
        const nbStableBtn = document.getElementById('nb-dl-stable');
        const nbStableVer = document.getElementById('nb-stable-ver');
        const nbSnapshotBtn = document.getElementById('nb-dl-snapshot');
        const nbSnapshotVer = document.getElementById('nb-snapshot-ver');

        // IntelliJ elements
        const intellijStableBtn = document.getElementById('intellij-dl-stable');
        const intellijStableVer = document.getElementById('intellij-stable-ver');
        const intellijSnapshotBtn = document.getElementById('intellij-dl-snapshot');
        const intellijSnapshotVer = document.getElementById('intellij-snapshot-ver');

        // Dynamic NBM interactive button on nb.html
        const nbDynamicDlBtn = document.getElementById('dynamic-dl-btn');

        if (!winSnapBtn && !macSnapBtn && !linSnapBtn && !winStableBtn && !macStableBtn && !linStableBtn && !nbStableBtn && !nbSnapshotBtn && !intellijStableBtn && !intellijSnapshotBtn && !nbDynamicDlBtn)
            return;

        try {
            // 1. Fetch latest snapshot release directly
            const snapResponse = await fetch('https://api.github.com/repos/anahata-os/anahata-asi/releases/tags/latest-snapshot');
            if (snapResponse.ok) {
                const latestRelease = await snapResponse.json();
                if (latestRelease && latestRelease.assets) {
                    const assets = latestRelease.assets;

                    const winAsset = assets.find(asset => asset.name.endsWith('-windows.zip'));
                    const macAsset = assets.find(asset => asset.name.endsWith('-macos.zip'));
                    const linAsset = assets.find(asset => asset.name.endsWith('-linux.tar.gz'));
                    const nbmAsset = assets.find(asset => asset.name.endsWith('.nbm'));
                    const intellijAsset = assets.find(asset => (asset.name.includes('anahata-asi-intellij') || asset.name.includes('uno-anahata-asi-intellij')) && asset.name.endsWith('.zip'));

                    if (winAsset && winSnapBtn) {
                        winSnapBtn.href = winAsset.browser_download_url;
                        const sizeMb = Math.round(winAsset.size / (1024 * 1024));
                        const sizeSpan = winSnapBtn.querySelector('span');
                        if (sizeSpan)
                            sizeSpan.textContent = `.zip (Portable) • ${sizeMb} MB`;
                    }
                    if (macAsset && macSnapBtn) {
                        macSnapBtn.href = macAsset.browser_download_url;
                        const sizeMb = Math.round(macAsset.size / (1024 * 1024));
                        const sizeSpan = macSnapBtn.querySelector('span');
                        if (sizeSpan)
                            sizeSpan.textContent = `.zip (App Bundle) • ${sizeMb} MB`;
                    }
                    if (linAsset && linSnapBtn) {
                        linSnapBtn.href = linAsset.browser_download_url;
                        const sizeMb = Math.round(linAsset.size / (1024 * 1024));
                        const sizeSpan = linSnapBtn.querySelector('span');
                        if (sizeSpan)
                            sizeSpan.textContent = `.tar.gz (Binary) • ${sizeMb} MB`;
                    }

                    // Extract the desktop snapshot version from filename dynamically
                    let snapVersion = "SNAPSHOT";
                    if (linAsset) {
                        const match = linAsset.name.match(/Anahata-ASI-Desktop-(.*?)-linux/);
                        if (match)
                            snapVersion = match[1];
                    }

                    if (deskSnapshotVer) {
                        deskSnapshotVer.textContent = snapVersion.startsWith("v") ? snapVersion : `v${snapVersion}`;
                    }

                    // Dynamic NBM snapshot resolver (split by target: 300 / 310)
                    window.nbNbmAssets = window.nbNbmAssets || { stable: {}, snapshot: {} };
                    window.nbNbmVersions = window.nbNbmVersions || { stable: {}, snapshot: {} };
                    assets.forEach(asset => {
                        if (asset.name.endsWith('.nbm')) {
                            const match = asset.name.match(/(?:anahata-asi-nb|uno-anahata-asi-nb)-(.*?)\.nbm/);
                            const verStr = match ? match[1] : "SNAPSHOT";
                            if (asset.name.includes('300')) {
                                window.nbNbmAssets.snapshot['300'] = asset.browser_download_url;
                                window.nbNbmVersions.snapshot['300'] = verStr;
                            } else if (asset.name.includes('310')) {
                                window.nbNbmAssets.snapshot['310'] = asset.browser_download_url;
                                window.nbNbmVersions.snapshot['310'] = verStr;
                            }
                        }
                    });

                    if (nbmAsset && nbSnapshotBtn) {
                        nbSnapshotBtn.href = nbmAsset.browser_download_url;
                        const match = nbmAsset.name.match(/(?:anahata-asi-nb|uno-anahata-asi-nb)-(.*?)\.nbm/);
                        const verStr = match ? match[1] : "SNAPSHOT";
                        if (nbSnapshotVer) {
                            nbSnapshotVer.textContent = verStr.startsWith("v") ? verStr : `v${verStr}`;
                        }
                    }

                    // Dynamic IntelliJ snapshot resolver
                    if (intellijAsset && intellijSnapshotBtn) {
                        intellijSnapshotBtn.href = intellijAsset.browser_download_url;
                        const match = intellijAsset.name.match(/(?:anahata-asi-intellij|uno-anahata-asi-intellij)-(.*?)\.zip/);
                        const verStr = match ? match[1] : "SNAPSHOT";
                        if (intellijSnapshotVer) {
                            intellijSnapshotVer.textContent = verStr.startsWith("v") ? verStr : `v${verStr}`;
                        }
                    }
                }
            }

            // 2. Fetch latest stable release tag & assets
            const stableResponse = await fetch('https://api.github.com/repos/anahata-os/anahata-asi/releases/latest');
            if (stableResponse.ok) {
                const stableRelease = await stableResponse.json();
                if (stableRelease) {
                    const stableTag = stableRelease.tag_name || "v1.0.0";
                    const formattedTag = stableTag.startsWith("v") ? stableTag : `v${stableTag}`;

                    if (deskStableVer) {
                        deskStableVer.textContent = formattedTag;
                    }
                    if (nbStableVer) {
                        nbStableVer.textContent = formattedTag;
                    }
                    if (intellijStableVer) {
                        intellijStableVer.textContent = formattedTag;
                    }

                    if (stableRelease.assets && stableRelease.assets.length > 0) {
                        const stableAssets = stableRelease.assets;
                        const winStableAsset = stableAssets.find(a => a.name.endsWith('-windows.zip'));
                        const macStableAsset = stableAssets.find(a => a.name.endsWith('-macos.zip'));
                        const linStableAsset = stableAssets.find(a => a.name.endsWith('-linux.tar.gz'));
                        const nbmStableAsset = stableAssets.find(a => a.name.endsWith('.nbm'));
                        const intellijStableAsset = stableAssets.find(a => (a.name.includes('anahata-asi-intellij') || a.name.includes('uno-anahata-asi-intellij')) && a.name.endsWith('.zip'));

                        if (winStableAsset && winStableBtn) {
                            winStableBtn.href = winStableAsset.browser_download_url;
                            const sizeMb = Math.round(winStableAsset.size / (1024 * 1024));
                            const sizeSpan = winStableBtn.querySelector('span');
                            if (sizeSpan)
                                sizeSpan.textContent = `.zip (Portable) • ${sizeMb} MB`;
                        }
                        if (macStableAsset && macStableBtn) {
                            macStableBtn.href = macStableAsset.browser_download_url;
                            const sizeMb = Math.round(macStableAsset.size / (1024 * 1024));
                            const sizeSpan = macStableBtn.querySelector('span');
                            if (sizeSpan)
                                sizeSpan.textContent = `.zip (App Bundle) • ${sizeMb} MB`;
                        }
                        if (linStableAsset && linStableBtn) {
                            linStableBtn.href = linStableAsset.browser_download_url;
                            const sizeMb = Math.round(linStableAsset.size / (1024 * 1024));
                            const sizeSpan = linStableBtn.querySelector('span');
                            if (sizeSpan)
                                sizeSpan.textContent = `.tar.gz (Binary) • ${sizeMb} MB`;
                        }
                        if (stableRelease.assets && stableRelease.assets.length > 0) {
                            window.nbNbmVersions = window.nbNbmVersions || { stable: {}, snapshot: {} };
                            const rawTag = stableRelease.tag_name ? stableRelease.tag_name.replace(/^v/, "") : "";
                            window.latestNbStableVer = rawTag;
                            stableRelease.assets.forEach(asset => {
                                if (asset.name.endsWith('.nbm')) {
                                    const match = asset.name.match(/(?:anahata-asi-nb|uno-anahata-asi-nb)-(.*?)\.nbm/);
                                    const verStr = match ? match[1] : rawTag;
                                    if (asset.name.includes('300')) {
                                        window.nbNbmAssets.stable['300'] = `https://repo1.maven.org/maven2/uno/anahata/anahata-asi-nb/${verStr}/anahata-asi-nb-${verStr}.nbm`;
                                        window.nbNbmVersions.stable['300'] = verStr;
                                    } else if (asset.name.includes('310')) {
                                        window.nbNbmAssets.stable['310'] = `https://repo1.maven.org/maven2/uno/anahata/anahata-asi-nb/${verStr}/anahata-asi-nb-${verStr}.nbm`;
                                        window.nbNbmVersions.stable['310'] = verStr;
                                    }
                                }
                            });

                            // Dynamically update the Update Center plugin download button
                            const ucDlBtn = document.getElementById('uc-dynamic-dl-btn');
                            const ucDlText = document.getElementById('uc-dynamic-dl-text');
                            if (ucDlBtn && rawTag) {
                                const ucAsset = stableRelease.assets.find(a => a.name.includes('anahata-asi-nb-uc') && a.name.endsWith('.nbm'));
                                if (ucAsset) {
                                    ucDlBtn.href = ucAsset.browser_download_url;
                                    const ucMatch = ucAsset.name.match(/(?:anahata-asi-nb-uc|uno-anahata-asi-nb-uc)-(.*?)\.nbm/);
                                    const ucVer = ucMatch ? ucMatch[1] : rawTag;
                                    if (ucDlText) ucDlText.textContent = `Download Update Center NBM v${ucVer}`;
                                } else {
                                    ucDlBtn.href = `https://repo1.maven.org/maven2/uno/anahata/anahata-asi-nb-uc/${rawTag}/anahata-asi-nb-uc-${rawTag}.nbm`;
                                    if (ucDlText) ucDlText.textContent = `Download Update Center NBM v${rawTag}`;
                                }
                            }
                        }

                        if (typeof updateUcDisplay === 'function') {
                            updateUcDisplay();
                        }
                        if (intellijStableAsset && intellijStableBtn) {
                            intellijStableBtn.href = intellijStableAsset.browser_download_url;
                        }
                    }
                }
            }
        } catch (error) {
            console.error('Error resolving dynamic asset URLs:', error);
        }
    };

    initScrollSpy();
    initDynamicDownloads();
});

/* --- Dynamic Live Support Chat Injection (Anahata ASI) --- */
(function () {
    var s1 = document.createElement("script"),
            s0 = document.getElementsByTagName("script")[0];
    s1.async = true;
    // Dynamic tawk.to live support chat widget for anahata.uno
    s1.src = 'https://embed.tawk.to/6a218f67b974371c3124fc61/1jq9hgbb2';
    s1.charset = 'UTF-8';
    s1.setAttribute('crossorigin', '*');
    s0.parentNode.insertBefore(s1, s0);
})();
