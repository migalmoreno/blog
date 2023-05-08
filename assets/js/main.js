function setCopyrightYear() {
  document.querySelector("#year").innerHTML = new Date().getFullYear()
}

function highlightCurrentRoute() {
  const rx = /\/+$/
  for (let el of document.querySelectorAll("nav ul a")) {
    if (el.href.replace(rx, "") === window.location.href.replace(rx, "")) {
      el.classList.add("current")
    }
  }
}

function init() {
  setCopyrightYear()
  highlightCurrentRoute()
}

init()
