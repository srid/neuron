---
title: "Chart.js"
---

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

[Chart.js](https://www.chartjs.org/docs/latest/charts/mixed.html)

```{.chartjs}
{"type":"bar","data":{"labels":["January","February","March","April"],"datasets":[{"label":"Bar Dataset","data":[10,20,30,40],"borderColor":"rgb(255, 99, 132)","backgroundColor":"rgba(255, 99, 132, 0.2)"},{"label":"Line Dataset","data":[50,50,50,50],"type":"line","fill":false,"borderColor":"rgb(54, 162, 235)"}]},"options":{"scales":{"yAxes":[{"ticks":{"beginAtZero":true}}]}}}
```

HTML
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

Markdown
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
