let search, index, selectedTags;

let searchResults = document.getElementById("search-results"); // ul element
let searchInput = document.getElementById("search-input");

// Create a zettel link from a zettel object
function makeZettelLink(zettel) {
  let zettelLink = document.createElement("span");
  zettelLink.classList.add("zettel-link");

  let idLink = document.createElement("span");
  idLink.classList.add("zettel-link-idlink");

  let actualLink = document.createElement("a");
  actualLink.href = zettel.id + ".html";
  actualLink.innerHTML = zettel.id;
  idLink.appendChild(actualLink);
  zettelLink.appendChild(idLink);

  let linkTitle = document.createElement("span");
  linkTitle.classList.add("zettel-link-title");
  linkTitle.innerHTML = zettel.title;
  zettelLink.appendChild(linkTitle);

  return zettelLink;
}

// Renders the results as a list
function renderResults(results) {
  searchResults.innerHTML = "";

  for (var i = 0, length = results.length; i < length; i++) {
    let zettel = results[i];
    let result = document.createElement("li");
    let zettelLink = makeZettelLink(zettel);
    result.appendChild(zettelLink);
    searchResults.appendChild(result);
  }
}

// Rebuild the search, eventually useful for advanced search
function rebuildSearchIndex() {
  search = new JsSearch.Search("id");
  search.tokenizer = new JsSearch.StopWordsTokenizer(
    new JsSearch.SimpleTokenizer()
  );
  search.indexStrategy = new JsSearch.AllSubstringsIndexStrategy();
  search.addIndex("id");
  search.addIndex("title");
  search.addIndex("tags");
  search.addDocuments(index.zettels);
}

function matchSelectedTags(zettel) {
  return selectedTags.every((tag) => {
    return zettel.tags.includes(tag);
  });
}

// Runs and renders the search
function runSearch() {
  let query = searchInput.value;
  let results;
  if (query == "") {
    results = index.zettels;
  } else {
    results = search.search(query);
  }
  results = results.filter(matchSelectedTags);
  renderResults(results);
}

// Fill possible dropdown values with all tags from index
// and initialize selection
function initializeTags() {
  $('#search-tags').dropdown({
    values: index.tags.map((tag) => {
      return {name: tag, value: tag, selected: selectedTags.includes(tag)};
    }),
    onChange: (value) => {
      selectedTags = value.split(',').filter((tag) => tag != "");
      runSearch();
    }
  });
}

// Initialize variables with URL parameters
function initializeSearchFromURL() {
  let url = new URL(window.location.href);
  let searchParams = new URLSearchParams(url.search);
  searchInput.value = searchParams.get("q");
  selectedTags = searchParams.getAll("tag");
  initializeTags();
  runSearch();
}

// Rerun the search at every change
searchInput.addEventListener("input", runSearch);

// Query index.json and build the search index
// TODO: allow this even in local (see CORS restriction)
let xmlhttp = new XMLHttpRequest();
xmlhttp.onreadystatechange = function () {
  if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
    index = JSON.parse(xmlhttp.responseText);
    rebuildSearchIndex();
    initializeSearchFromURL();
  }
};
xmlhttp.open("GET", "index.json", true);
xmlhttp.send();

$('#search-tags').dropdown();
