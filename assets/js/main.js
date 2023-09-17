const setCopyrightYear = () => {
  document.querySelector(".footer__year").innerHTML = new Date().getFullYear()
}

const highlightCurrentRoute = () => {
  const routePath = window.location.pathname.split("/").join("")
  for (let el of document.querySelectorAll(".menu-item__link")) {
    const linkPath = el.pathname.split("/").join("")
    if (linkPath === routePath || linkPath && routePath.includes(linkPath)) {
      el.classList.add("menu-item__link--selected")
    }
  }
}

(function init() {
  setCopyrightYear()
  highlightCurrentRoute()
})()
