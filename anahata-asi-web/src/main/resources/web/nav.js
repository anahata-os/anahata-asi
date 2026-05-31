/* Shared Navigation Component for Anahata ASI Web */
document.addEventListener('DOMContentLoaded', () => {
    const navPlaceholder = document.getElementById('main-nav');
    if (!navPlaceholder) return;

    const currentPath = window.location.pathname;
    const isIndex = currentPath.endsWith('index.html') || currentPath.endsWith('/');
    
    // Detection logic
    const activeFile = currentPath.split('/').pop();
    const isActive = (file) => activeFile === file;

    navPlaceholder.innerHTML = `
        <nav class="${isIndex ? '' : 'nav-solid'}">
            <div class="nav-container container">
                <a href="index.html" class="logo">
                    <img src="assets/logo-transparent.png" alt="Anahata Logo" style="height: 40px;">
                </a>
                <div class="menu-toggle"><i class="fas fa-bars"></i></div>
                <div class="nav-links">
                    <a href="index.html" class="${isIndex ? 'active-link' : ''}">Home</a>
                    
                    <div class="dropdown">
                        <a href="#" class="dropdown-toggle ${(isActive('core.html') || isActive('swing.html') || isActive('yam.html') || isActive('gemini.html') || isActive('openai.html') || isActive('anthropic.html') || isActive('compatible.html')) ? 'active-link' : ''}">
                            Modules <i class="fas fa-chevron-down"></i>
                        </a>
                        <div class="dropdown-menu">
                            <a href="core.html" class="${isActive('core.html') ? 'active-item' : ''}"><i class="fas fa-brain"></i> Core API</a>
                            <a href="swing.html" class="${isActive('swing.html') ? 'active-item' : ''}"><i class="fas fa-desktop"></i> Swing UI</a>
                            <a href="yam.html" class="${isActive('yam.html') ? 'active-item' : ''}"><i class="fas fa-flask"></i> Yam Tools</a>
                            <div style="border-top: 1px solid rgba(255,255,255,0.1); margin: 5px 0;"></div>
                            <a href="gemini.html" class="${isActive('gemini.html') ? 'active-item' : ''}"><i class="fas fa-plug"></i> Gemini Provider</a>
                            <a href="openai.html" class="${isActive('openai.html') ? 'active-item' : ''}"><i class="fas fa-bolt"></i> OpenAI Provider</a>
                            <a href="anthropic.html" class="${isActive('anthropic.html') ? 'active-item' : ''}"><i class="fas fa-ghost"></i> Anthropic Provider</a>
                            <a href="compatible.html" class="${isActive('compatible.html') ? 'active-item' : ''}"><i class="fas fa-globe"></i> Universal Alliance</a>
                        </div>
                    </div>

                    <div class="dropdown">
                        <a href="#" class="dropdown-toggle ${(isActive('nb.html') || isActive('desktop.html')) ? 'active-link' : ''}">
                            Applications <i class="fas fa-chevron-down"></i>
                        </a>
                        <div class="dropdown-menu">
                            <a href="nb.html" class="${isActive('nb.html') ? 'active-item' : ''}"><i class="fas fa-code"></i> NetBeans ASI Studio</a>
                            <a href="desktop.html" class="${isActive('desktop.html') ? 'active-item' : ''}"><i class="fas fa-rocket"></i> Anahata ASI Desktop</a>
                        </div>
                    </div>

                    <a href="quickstart.html" class="${isActive('quickstart.html') ? 'active-link' : ''}">Quick Start</a>
                    <a href="apidocs/index.html">Javadocs</a>
                    
                    <div class="social-links">
                        <a href="https://www.youtube.com/@anahata108" target="_blank" title="Anahata TV"><i class="fab fa-youtube"></i></a>
                        <a href="https://discord.gg/gwGWWxPUXE" target="_blank" title="Discord"><i class="fab fa-discord"></i></a>
                        <a href="https://x.com/AnahataASI" target="_blank" title="Twitter / X"><i class="fa-brands fa-x-twitter"></i></a>
                        <a href="https://github.com/anahata-os/anahata-asi" target="_blank" title="GitHub"><i class="fab fa-github"></i></a>
                    </div>
                    <a href="https://github.com/sponsors/anahata-os" class="btn-sponsor">Sponsor</a>
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

        if (sidebarLinks.length === 0 || sections.length === 0) return;

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
            if (slides.length === 0) return;
            if (n > slides.length) {slideIndex = 1}    
            if (n < 1) {slideIndex = slides.length}
            for (i = 0; i < slides.length; i++) {
                slides[i].style.display = "none";  
            }
            slides[slideIndex-1].style.display = "block";  
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
        const winBtn = document.getElementById('dl-windows');
        const macBtn = document.getElementById('dl-macos');
        const linBtn = document.getElementById('dl-linux');
        if (!winBtn && !macBtn && !linBtn) return;

        try {
            const response = await fetch('https://api.github.com/repos/anahata-os/anahata-asi/releases');
            if (!response.ok) throw new Error('Failed to fetch releases metadata');
            
            const releases = await response.json();
            if (!releases || releases.length === 0) return;

            // Find the most recent release candidate or stable release (ignoring rolling snapshot tags)
            const latestRelease = releases.find(rel => rel.tag_name !== 'latest-snapshot' && !rel.draft);
            if (!latestRelease) return;

            const assets = latestRelease.assets;
            
            const winAsset = assets.find(asset => asset.name.endsWith('-windows.zip'));
            const macAsset = assets.find(asset => asset.name.endsWith('-macos.zip'));
            const linAsset = assets.find(asset => asset.name.endsWith('-linux.tar.gz'));

            if (winAsset && winBtn) {
                winBtn.href = winAsset.browser_download_url;
                const sizeMb = Math.round(winAsset.size / (1024 * 1024));
                winBtn.querySelector('span').textContent = `.zip (Portable) • ${sizeMb} MB`;
            }
            if (macAsset && macBtn) {
                macBtn.href = macAsset.browser_download_url;
                const sizeMb = Math.round(macAsset.size / (1024 * 1024));
                macBtn.querySelector('span').textContent = `.zip (App Bundle) • ${sizeMb} MB`;
            }
            if (linAsset && linBtn) {
                linBtn.href = linAsset.browser_download_url;
                const sizeMb = Math.round(linAsset.size / (1024 * 1024));
                linBtn.querySelector('span').textContent = `.tar.gz (Binary) • ${sizeMb} MB`;
            }

            const subtitle = document.querySelector('#installation p');
            if (subtitle) {
                subtitle.innerHTML = `Native standalone binaries are compiled on secure runners. Currently serving the latest release candidate: <strong style="color: var(--barca-gold); font-family: 'JetBrains Mono', monospace;">${latestRelease.tag_name}</strong>.`;
            }
        } catch (error) {
            console.error('Error resolving dynamic asset URLs:', error);
        }
    };

    initScrollSpy();
    initDynamicDownloads();
});
