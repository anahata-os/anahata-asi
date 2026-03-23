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
                        <a href="#" class="dropdown-toggle ${(isActive('core.html') || isActive('swing.html') || isActive('yam.html') || isActive('gemini.html')) ? 'active-link' : ''}">
                            Libraries <i class="fas fa-chevron-down"></i>
                        </a>
                        <div class="dropdown-menu">
                            <a href="core.html" class="${isActive('core.html') ? 'active-item' : ''}"><i class="fas fa-brain"></i> Core API</a>
                            <a href="swing.html" class="${isActive('swing.html') ? 'active-item' : ''}"><i class="fas fa-desktop"></i> Swing UI</a>
                            <a href="yam.html" class="${isActive('yam.html') ? 'active-item' : ''}"><i class="fas fa-flask"></i> Yam Tools</a>
                            <a href="gemini.html" class="${isActive('gemini.html') ? 'active-item' : ''}"><i class="fas fa-plug"></i> Gemini Provider</a>
                        </div>
                    </div>

                    <div class="dropdown">
                        <a href="#" class="dropdown-toggle ${(isActive('nb.html') || isActive('standalone.html')) ? 'active-link' : ''}">
                            Applications <i class="fas fa-chevron-down"></i>
                        </a>
                        <div class="dropdown-menu">
                            <a href="nb.html" class="${isActive('nb.html') ? 'active-item' : ''}"><i class="fas fa-code"></i> NetBeans ASI Studio</a>
                            <a href="standalone.html" class="${isActive('standalone.html') ? 'active-item' : ''}"><i class="fas fa-rocket"></i> Anahata ASI Desktop</a>
                        </div>
                    </div>

                    <a href="apidocs/index.html">Javadocs</a>
                    
                    <div class="social-links">
                        <a href="https://www.youtube.com/@anahata108" target="_blank" title="Anahata TV"><i class="fab fa-youtube"></i></a>
                        <a href="https://discord.gg/Pjev7Cha" target="_blank" title="Discord"><i class="fab fa-discord"></i></a>
                        <a href="https://x.com/AnahataASI" target="_blank" title="Twitter / X"><i class="fa-brands fa-x-twitter"></i></a>
                        <a href="https://github.com/anahata-os/anahata-asi" target="_blank" title="GitHub"><i class="fab fa-github"></i></a>
                    </div>
                    <a href="https://github.com/anahata-os/sponsors" class="btn-sponsor">Sponsor</a>
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

    initScrollSpy();
});
