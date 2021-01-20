---
tags: [custom-head-recipe]
---

# Chart.js

[Chart.js](https://www.chartjs.org/) is an easy way to include animated, interactive graphs on your website for free.

Render mermaid content in neuron by including the following HTML snippet in your `head.html` file ([[Custom JavaScript and CSS]]) or the zettel file ([[raw-html]]):

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.4/Chart.bundle.min.js" integrity="sha512-SuxO9djzjML6b9w9/I07IWnLnQhgyYVSpHZx0JV97kGBfTIsUYlWflyuW4ypnvhBrslz1yJ3R+S14fdCWmSmSA==" crossorigin="anonymous"></script>
<script>
window.addEventListener("load", function(){
  for (let element of document.getElementsByClassName("chartjs")) {
    let parent = element.parentNode
    let pparent = parent.parentNode
    let canvas = document.createElement('canvas');
    let box = document.createElement('div');
    box.appendChild(canvas);
    let ctx = canvas.getContext("2d")
    let myChart = new Chart(ctx, JSON.parse(element.textContent));
    box.setAttribute("style","display:block;width:75%;text-align:'center';margin: 5px auto;");
    pparent.replaceChild(box, parent)
  }
});
</script>
```

Then include your chart data in Markdown as follows:

~~~markdown
```chartjs
{
  "type": "bar",
  "data": {
    "labels": [
      "January",
      "February",
      "March",
      "April"
    ],
    "datasets": [
      {
        "label": "Bar Dataset",
        "data": [
          10,
          20,
          30,
          40
        ],
        "borderColor": "rgb(255, 99, 132)",
        "backgroundColor": "rgba(255, 99, 132, 0.2)"
      },
      {
        "label": "Line Dataset",
        "data": [
          50,
          50,
          50,
          50
        ],
        "type": "line",
        "fill": false,
        "borderColor": "rgb(54, 162, 235)"
      }
    ]
  },
  "options": {
    "scales": {
      "yAxes": [
        {
          "ticks": {
            "beginAtZero": true
          }
        }
      ]
    }
  }
}
```
~~~

When you open your generated neuron site, it will render as follows:

```{.chartjs}
{"type":"bar","data":{"labels":["January","February","March","April"],"datasets":[{"label":"Bar Dataset","data":[10,20,30,40],"borderColor":"rgb(255, 99, 132)","backgroundColor":"rgba(255, 99, 132, 0.2)"},{"label":"Line Dataset","data":[50,50,50,50],"type":"line","fill":false,"borderColor":"rgb(54, 162, 235)"}]},"options":{"scales":{"yAxes":[{"ticks":{"beginAtZero":true}}]}}}
```


<!-- Usually this goes to head.html, but we include it here because we don't want the JS to run on other note files. -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.4/Chart.bundle.min.js" integrity="sha512-SuxO9djzjML6b9w9/I07IWnLnQhgyYVSpHZx0JV97kGBfTIsUYlWflyuW4ypnvhBrslz1yJ3R+S14fdCWmSmSA==" crossorigin="anonymous"></script>
<script>
window.addEventListener("load", function(){
  for (let element of document.getElementsByClassName("chartjs")) {
    let parent = element.parentNode
    let pparent = parent.parentNode
    let canvas = document.createElement('canvas');
    let box = document.createElement('div');
    box.appendChild(canvas);
    let ctx = canvas.getContext("2d")
    let myChart = new Chart(ctx, JSON.parse(element.textContent));
    box.setAttribute("style","display:block;width:75%;text-align:'center';margin: 5px auto;");
    pparent.replaceChild(box, parent)
  }
});
</script>

