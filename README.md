# animint2 Gallery

[animint2](https://cran.r-project.org/web/packages/animint2/) is an R package for animated and interactive data visualizations. This repository is for the [animint2 gallery](https://animint.github.io/gallery/), which showcases examples of what `animint2` can do. Additions to the gallery are encouraged.


## How to Contribute

### Make a Data Viz

1. First, run `install.packages("animint2")` in R to get the most recent version from CRAN (>= 2023.11.21). The most recent versions support the `source` option for the `animint()` function, which is required to publish an animint to a gallery.
2. Create a data `viz = animint(ggplots, title="data viz title", source="https://link.to/your_code.R")` object with options:
  * `title`: Replace the example text with a string describing the data viz.
  * `source`: Replace the example URL with the URL of your data viz source code.
3. Run `animint2pages(viz, "new_github_repo")` to create a new GitHub repository with your viz deployed in the gh-pages branch. For more detailed instructions, see [the subsection about publishing to GitHub Pages in Chapter 5 of The animint2 Manual](https://rcdata.nau.edu/genomic-ml/animint2-manual/Ch05-sharing.html#pages).
4. Take a screenshot of the data viz, name it `Capture.PNG`, and add it to the gh-pages branch of that repo.

### Add your Data Viz to the Gallery

In the following instructions, replace `YOUR_GITHUB_USERNAME` with your GitHub username.

1. If you have not yet forked the gallery, then [fork the gallery](https://github.com/animint/gallery/fork). Make sure your fork includes the `gh-pages` branch. This makes a copy of the animint2 gallery and places it on your list of repositories.
2. If you have not yet cloned your fork, then clone it with `git clone git@github.com:YOUR_GITHUB_USERNAME/gallery ~/R/gallery`. This copies your fork from GitHub onto your local machine.
3. Add `YOUR_GITHUB_USERNAME/new_github_repo` on a new line in `repos.txt`. The `new_github_repo` should be the same name that you used when you published a data viz `animint2pages`.
4. Run `animint2::update_gallery()` to update and push the changes to the `gh-pages` branch of your fork.
5. After a few minutes, your updated gallery will appear on https://YOUR_GITHUB_USERNAME.github.io/gallery/.
6. Open a pull request from your fork's `gh-pages` branch (head/compare) into the animint/gallery `gh-pages` branch (base) by clicking the "Contribute" button on your fork's GitHub web page or via a URL like https://github.com/animint/gallery/compare/gh-pages...YOUR_GITHUB_USERNAME:gallery:gh-pages?expand=1.
7. Make sure your pull request description has a link to the rendered version of your fork of the gallery, such as https://YOUR_GITHUB_USERNAME.github.io/gallery/. That way, reviewers can quickly preview what the new gallery would look like after merging the pull request.

For more info about galleries, go to [Chapter 5 of The animint2 Manual and read the subsection on organizing animints in a gallery](https://rcdata.nau.edu/genomic-ml/animint2-manual/Ch05-sharing.html#gallery).
