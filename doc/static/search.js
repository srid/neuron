'use strict';

function buildIndex() {
  if (window.NEURON_SEARCH_INDEX)
    return

  window.fetch('/cache.json')
    .then(r => r.json())
    .then(cache => {
      configureIndex()

      const { Graph: graph } = cache
      const { vertices: documents } = graph

      Object.values(documents).forEach(doc => {
        const {
          ID: id,
          Title: title,
          Slug: slug,
          Date: date,
          Meta: {
            tags
          }
        } = doc

        window.NEURON_SEARCH_INDEX.addDoc({
          id,
          title,
          date,
          tags,
          slug
        })
      })
    })
}

function configureIndex() {
  window.NEURON_SEARCH_INDEX = elasticlunr(function() {
    this.addField('id')
    this.addField('slug')
    this.addField('title')
    this.addField('tags')
    this.addField('body')

    this.setRef('id')
  })
}

function insertSearchBar() {
  const zettelContainerNode = document.querySelector('.container.universe')
  const firstChild = zettelContainerNode.firstElementChild

  const searchContainerNode = document.createElement('div')
  searchContainerNode.setAttribute('id', 'search-bar')
  searchContainerNode.setAttribute('class', 'zettel-view')
  searchContainerNode.setAttribute('style', 'margin-bottom: 20px;')

  const searchBar = document.createElement('div')
  searchBar.setAttribute('class', 'ui input')

  searchContainerNode.appendChild(searchBar)

  const searchInput = document.createElement('input')
  searchInput.setAttribute('type', 'text')
  searchInput.setAttribute('placeholder', 'search')

  searchBar.appendChild(searchInput)

  zettelContainerNode.insertBefore(searchContainerNode, firstChild)

  searchInput.addEventListener('input', debounce(event => handleSearch(event)))
}

function handleSearch(event) {
  const searchParam = event.target.value

  if (typeof searchParam !== 'string' || searchParam.length < 3)
    return

  const results = window.NEURON_SEARCH_INDEX.search(event.target.value)

  console.log(results)
}

function debounce(handler, timeout = 300){
  let timer

  return (...args) => {
    clearTimeout(timer)

    timer = setTimeout(() => {
      handler.apply(this, args)
    }, timeout)
  }
}

document.addEventListener('DOMContentLoaded', buildIndex)
document.addEventListener('DOMContentLoaded', insertSearchBar)
