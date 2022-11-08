'use strict';

const searchOptions = {
  bool: "AND",
  expand: true,
  fields: {
    title: { boost: 2 },
    tags: { boost: 2 },
    id: { boost: 1 },
    slug: { boost: 0 }
  }
}

const tagSearchOptions = Object.assign({}, searchOptions, {
  fields: {
    title: { boost: 0 },
    tags: { boost: 1 },
    id: { boost: 0 },
    slug: { boost: 0 }
  }
})

function buildIndex() {
  if (window.NEURON_SEARCH_INDEX)
    return

  window.fetch('/static/large-cache.json')
    .then(r => r.json())
    .then(cache => {
      configureIndex()

      const { Graph: { vertices: documents } } = cache

      Object.values(documents).forEach(doc => {
        const {
          ID: id,
          Title: title,
          Slug: slug,
          Meta: {
            tags
          }
        } = doc

        window.NEURON_SEARCH_INDEX.addDoc({
          id,
          title,
          tags,
          slug
        })
      })
    })
    .then(handleUrlSearchParams)
}

function configureIndex() {
  if (window.NEURON_SEARCH_INDEX !== undefined)
    return

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
  const pageContainer = document.querySelector('#zettel-container').parentElement
  const firstChild = pageContainer.firstElementChild

  const searchBarContainer = document.createElement('div')
  searchBarContainer.setAttribute('class', 'text ui container')

  const searchBar = document.createElement('div')
  searchBar.setAttribute('class', 'zettel-view events-container')
  searchBar.setAttribute('style', 'margin-bottom: 20px')

  searchBarContainer.appendChild(searchBar)

  const article = document.createElement('article')
  article.setAttribute('class', 'ui raised attached segment zettel-content')

  searchBar.appendChild(article)

  const pandocContainer = document.createElement('div')
  pandocContainer.setAttribute('class', 'pandoc')

  article.appendChild(pandocContainer)

  const searchBox = document.createElement('div')
  searchBox.setAttribute('id', 'search-box')

  const inputContainer = document.createElement('div')
  inputContainer.setAttribute('class', 'ui fluid icon input')

  const input = document.createElement('input')
  input.setAttribute('type', 'text')
  input.setAttribute('placeholder', 'Search...')
  input.setAttribute('id', 'search-input')

  const searchIcon = document.createElement('i')
  searchIcon.setAttribute('class', 'search icon')

  inputContainer.appendChild(input)
  inputContainer.appendChild(searchIcon)

  searchBox.appendChild(inputContainer)

  const searchResults = document.createElement('ul')
  searchResults.setAttribute('id', 'search-results')

  searchBox.appendChild(searchResults)

  pandocContainer.appendChild(searchBox)

  pageContainer.insertBefore(searchBarContainer, firstChild)

  bindToSearchBox()
}

function bindToSearchBox() {
  const searchBox = document.querySelector('#search-box')

  if (searchBox === null)
    return

  const searchInput = searchBox.querySelector('#search-input')
  const resultsContainer = searchBox.querySelector('#search-results')

  searchInput.addEventListener('input', debounce(event => handleSearchEvent(event, resultsContainer)))
}

function handleSearchEvent(event, resultsContainer) {
  handleSearch(event.target.value, resultsContainer)
}

// Currently only handles single tag, or general search
function handleSearch(query, resultsContainer) {
  if (typeof query !== 'string')
    return

  const tagSearchRegexp = /tag:([\w-/]+)/
  const tagQuery = query.match(tagSearchRegexp)

  let results

  if (tagQuery === null)
    results = window.NEURON_SEARCH_INDEX.search(query, searchOptions)
  else
    results = window.NEURON_SEARCH_INDEX.search(tagQuery[1], tagSearchOptions)

  displayResults(results, resultsContainer)
}

/***
 * This is not very efficient, especially I think for large
 * lists of results.
 *
 * An improvement would be to take an approach similar
 * to D3's enter/exit paradigm, and only render new HTML
 * nodes for *new* entries in the result list, and only
 * remove HTML for those entries that have fallen out.
 *
 * It might even just be best to use D3, or a limited import
 * if not the entire project.
 *
 * Same goes for React/Vue/etc, though given the small
 * scope here D3 seems more appropriate and lightweight,
 * though hand-rolled may still be best.
 */
function displayResults(newResults, resultsContainer) {
  const children = newResults.map(createResultHtml)

  resultsContainer.replaceChildren(...children)
}

function createResultHtml(result) {
  const { doc } = result

  const li = document.createElement('li')
  li.setAttribute('class', 'ui list search-result-entry')

  const title = document.createElement('h3')
  title.setAttribute('class', 'ui header search-result-entry--title')
  const titleLink = document.createElement('a')
  titleLink.setAttribute('href', `/${ doc.slug }.html`)
  titleLink.innerText = doc.title
  title.appendChild(titleLink)

  li.appendChild(title)

  const relevancyScore = document.createElement('div')
  relevancyScore.setAttribute('class', 'search-result-entry--relevancy-score')
  relevancyScore.innerHTML = `Relevancy: <strong>${ Number(result.score).toFixed(1) }`

  li.appendChild(relevancyScore)

  const tags = document.createElement('div')
  tags.setAttribute('class', 'search-result-entry--tags')
  const tagsTitle = document.createElement('h5')
  //tagsTitle.setAttribute('class', 'ui header')
  tagsTitle.innerText = 'Tags'
  tags.appendChild(tagsTitle)
  const tagsList = document.createElement('div')
  tagsList.setAttribute('class', 'ui horizontal list')
  tags.appendChild(tagsList)
  doc.tags.forEach(tag => {
    const tagItem = document.createElement('div')
    tagItem.setAttribute('class', 'item')

    const tagItemLink = document.createElement('a')

    const searchParam = new URLSearchParams()
    searchParam.set('search[tags]', `["${ tag }"]`)
    const paramString = searchParam.toString()

    tagItemLink.setAttribute('href', `${ window.location.pathname }?${ paramString }`)
    tagItemLink.innerText = `#${ tag }`

    tagItem.appendChild(tagItemLink)

    tagsList.appendChild(tagItem)
  })

  li.appendChild(tags)

  return li
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

function handleUrlSearchParams() {
  const searchParams = new URLSearchParams(window.location.search)

  if (searchParams.keys().length === 0)
    return

  const searchParamsKeys = [ ...searchParams.keys() ]
  const resultsContainer = document.querySelector('#search-results')

  if (searchParamsKeys.includes('q')) {
    const query = searchParams.get('q')

    const searchInput = document.querySelector('#search-input')

    searchInput.value = query

    handleSearch(query, resultsContainer)
  } else
    return
}

function handleTagSearch(tagsParam, resultsContainer) {
  const tags = JSON.parse(tagsParam)

  const result = window.NEURON_SEARCH_INDEX.fieldSearch(tags, 'tags', {
    tags: {}
  })
  //const result = window.NEURON_SEARCH_INDEX.search(tags.join(' '), {
    //fields: {
      //tags: {
        //boost: 2
      //}
    //}
  //})

  console.log(tags, result)
}

document.addEventListener('DOMContentLoaded', buildIndex)
document.addEventListener('DOMContentLoaded', insertSearchBar)
