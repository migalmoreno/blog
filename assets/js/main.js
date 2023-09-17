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

const setPortfolioMetadata = async () => {
  const API_URL = "https://api.github.com/repos/migalmoreno/"
  const portfolioContainer = document.querySelector(".portfolio")
  if (portfolioContainer) {
    const projectItems = portfolioContainer.querySelectorAll(".project-item")
    const projectItemsStars = []
    for (let project of projectItems) {
      const projectName = project.querySelector(".project-item__title").innerHTML
      const starIcon = project.querySelector(".icon__star")
      try {
        const res = await fetch(API_URL + projectName)
        const data = await res.json()
        if (data) {
          projectItemsStars.push(data.stargazers_count)
        } else {
          starIcon.style.display = "none"
        }
      } catch (err) {
        starIcon.style.display = "none"
        throw(err)
      }
    }
    if (projectItemsStars.length > 0) {
      for (let [i, project] of projectItems.entries()) {
        project.querySelector(".project-item__stargazers").innerHTML = projectItemsStars[i]
      }
    }
  }
}

(() => {
  setCopyrightYear()
  highlightCurrentRoute()
  setPortfolioMetadata()
})()
