# Configuration

You may configure the parameters of your web interface by adding an optional configuration file named `neuron.dhall` under the notes directory. It should contain:

## Supported fields

{.ui .table}
| Field name        | Description                                                                                                                                                                                |
|-------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`siteTitle`**   | Your Neuron site's title                                                                                                                                                                   |
| **`siteBaseUrl`** | The base URL of your published Neuron site. Setting the base URL will enable [breadcrumbs](https://developers.google.com/search/docs/data-types/breadcrumb) in your site's structured data |
| **`author`**      | Author name                                                                                                                                                                                |
| **`theme`**       | Color scheme to use for your site. Value must be [one of the color names](https://semantic-ui.com/usage/theming.html#sitewide-defaults) supported by SemanticUI.                           |
| **`editUrl`**     | The URL (without the zettel filename) to edit zettels. To remove the edit button from the navbar, remove this entry from your configuration.                                               |
| **`plugins`**     | See [[Plugins]]                                                                                                                                                                            |
## Example 

```json
{ siteTitle =
    "My Zettelkasten for college"
, siteBaseUrl =
    Some "https://somecollege.edu/~john/neuron"
, theme =
    "brown"
, editUrl =
    Some "https://github.com/john/website/edit/master/notes/"
}
```

