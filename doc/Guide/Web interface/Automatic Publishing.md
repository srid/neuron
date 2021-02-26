---
slug: publishing
---

You can configure your neuron notes to be **automatically published** on the web. The simplest approach to achieve this is to store your notes in GitHub[^gitlab], and use their GitHub Pages + GitHub Actions[^this]. You can give this a try instantly with zero-configuration by using [`neuron-template`](https://github.com/srid/neuron-template). Follow the instructions in that link to get started. 

In order to enable automatic publishing for your *existing* neuron site that is already on GitHub, begin by copying `neuron-template`'s [`.github/workflows`](https://github.com/srid/neuron-template/tree/master/.github/workflows) directory to your repository.

[^this]: This very site is set up to automatically publish in this manner.

[^gitlab]: If you prefer to use GitLab (and thus GitLab Pages + GitLab CI) over GitHub, checkout the unofficial template [thematten/neuron-template](https://gitlab.com/thematten/neuron-template) on GitLab. For an existing GitLab repo, you will want to copy over its [`.gitlab-ci.yml`](https://gitlab.com/thematten/neuron-template/-/blob/master/.gitlab-ci.yml).
