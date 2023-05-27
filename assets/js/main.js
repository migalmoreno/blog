function setCopyrightYear() {
  document.querySelector(".footer__year").innerHTML = new Date().getFullYear()
}

function highlightCurrentRoute() {
  const rx = /\/+$/
  for (let el of document.querySelectorAll(".menu-item__link")) {
    if (el.href.replace(rx, "") === window.location.href.replace(rx, "")) {
      el.classList.add("menu-item__link--selected")
    }
  }
}

(function init() {
  setCopyrightYear()
  highlightCurrentRoute()
})()
