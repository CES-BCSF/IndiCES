// Menu hamburguesa
document.addEventListener("DOMContentLoaded", function () {
  var btn = document.getElementById("hamburger-btn");
  var nav = document.getElementById("nav-mobile");

  if (btn && nav) {
    btn.addEventListener("click", function () {
      btn.classList.toggle("active");
      nav.classList.toggle("open");
    });

    // Cerrar menu al hacer click en un link
    nav.querySelectorAll("a").forEach(function (link) {
      link.addEventListener("click", function () {
        btn.classList.remove("active");
        nav.classList.remove("open");
      });
    });
  }

  // Scrollspy
  var sections = document.querySelectorAll("section[id]");
  var navLinks = document.querySelectorAll(".nav-desktop a[href^='#'], .nav-mobile a[href^='#']");

  if (sections.length && navLinks.length) {
    var observer = new IntersectionObserver(function (entries) {
      entries.forEach(function (entry) {
        if (entry.isIntersecting) {
          navLinks.forEach(function (link) {
            link.classList.toggle(
              "nav-active",
              link.getAttribute("href") === "#" + entry.target.id
            );
          });
        }
      });
    }, {
      rootMargin: "-40% 0px -55% 0px"
    });

    sections.forEach(function (section) {
      observer.observe(section);
    });
  }
});
