# Generating multiple sites

In order to support subset as a blog (see [#222](https://github.com/srid/neuron/issues/222)), we need the ability to configure multiple sites (e.g. one for your local notes and another for your blog) as well as the ability to publish only a subset of the Zettelkasten (e.g. your blog might only publish zettels tagged `blog`).

## Filtering zettels while generating

For this, we add a new config parameter `filterZettels` with the same syntax as we use for queries. Only zettel matching this query will be included while generating the site.

## Multiple site configuration

To configure multiple sites, we modify the existing configuration format.

- At the top level, we have the following fields:
|Field name|Description|
|---|---|
|**`minVersion`**|The minimum neuron version your site must be generated with. (optional)|
|**`sites`**|A mapping of site identifier and site config.|

- Each site config can have these optional parameters:
|Field name|Description|
|---|---|
|**`siteTitle`**|Your Neuron site's title|
|**`author`**|Author name|
|**`siteBaseUrl`**|The base URL of your published Neuron site. Setting the base URL will enable [breadcrumbs](https://developers.google.com/search/docs/data-types/breadcrumb) in your site's structured data|
|**`theme`**|Color scheme to use for your site. Value must be [one of the color names](https://semantic-ui.com/usage/theming.html#sitewide-defaults) supported by SemanticUI.|
|**`aliases`**|Setup custom redirects in the statically generated site.|
|**`editUrl`**|The URL (without the zettel filename) to edit zettels.|
|**`formats`**|Formats other than Markdown to use; eg: `["markdown", "org"]`|
|**`filterZettels`**|Query used to filter the zettels|

For backwards compatability, in absence of the `sites` field, we conside a single site implictly named `default`. All fields that are allowed at the top level (in the new config format) as well as for individual site config can be used here.

## Example
```json
{ minVersion = "0.4.0.0"
, sites =
  { default =
    { siteTitle = "My local Zettelkasten"
    , siteBaseUrl = Some "127.0.0.1:8080"
    , theme = "brown"
    }
  , blog =
    { siteTitle = "My blog"
    , siteBaseUrl = Some "https://somecollege.edu/~john/neuron"
    , theme = "blue"
    , aliases = [ "thesis-portal:2011402" ]
    , editUrl = Some "https://github.com/john/website/edit/master/notes/"
    , filterZettels = "<z:zettels?tag=blog>"
    }
  }
}
```
