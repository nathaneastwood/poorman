# Contributing

Contributions via GitHub pull requests are gladly accepted from their original author. By participating in this project, you agree to abide by the thoughtbot [code of conduct](https://thoughtbot.com/open-source-code-of-conduct).

* [Licensing](#license)
* [R Coding Standards](#r)
* [git Commit Standards](#git)

The [pull request template](./pull_request_template.md) will guide you through what is expected of a pull request for this project.

## <a name="license"></a> Licensing

Along with any pull requests, please state that the contribution is your original work and that you license the work to the project under the project's open source license. Whether or not you state this explicitly, by submitting any copyrighted material via pull request, email, or other means you agree to license the material under the project's open source license and warrant that you have the legal authority to do so.

## <a name="r"></a> R Coding Standards

We follow the style guide maintained within the [tidyverse](http://style.tidyverse.org). This is tested using the [`lintr`](https://github.com/jimhester/lintr) package; you can automatically conform your code to these standards using the [`styler`](https://github.com/r-lib/styler) package. Please quality any integers with `L`, e.g. `1L`.

## <a name="git"></a> git Commit Standards

We follow the commit message style guide maintained within the [angular.js](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#-git-commit-guidelines) project.

The start of commit messages must be one of the following:

* **feat**: A new feature
* **fix**: A bug fix
* **doc**: Documentation only changes
* **style**: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
* **refactor**: A code change that neither fixes a bug or adds a feature
* **perf**: A code change that improves performance
* **test**: Adding missing tests
* **chore**: Changes to the build process or auxiliary tools and libraries such as documentation generation

Do not capitalise the first letter.
