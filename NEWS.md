# ss3diags 2.0.3

* Changed r4ss dependency to 1.44.0 (CRAN version) and above. 
* Replaced default R CMD CHECK workflow with NOAA-fish-tools/ghactions4r's version.
* Fixed Ensemble Quantile Shading Issues (#4)
* Replaced the example datasets in the ss3diags cookbook, and the handbook with the current "simple" ensemble model.
* Fixup minor code documentation formatting issues.
* Implemented NOAA-fish-tools/ghactions4r integrated code styler and documentation for pull requests.
  * SSmase and SSplotHcxval: Fixed up code styler's dollar subset conversion issues. example:`message('x$name is 1,2,3,4')`

# ss3diags 2.0.2

* Changed r4ss CRAN version (1.36.1) dependency to development version
* Refactored common ssplot subfunctions, renamed, and used equilvants in r4ss util 
  * `legendfun` -> `r4ss::add_legend`
  * `pngfun` -> `r4ss::save_png`
  * `rc` -> `r4ss::rich_colors_short`
  * `sspar` -> `r4ss::sspar`
* Fix SSplotRetro.R indention causing issues with code styler

# ss3diags 2.0.1

* Minor README Updates and corrections
  * Change in SSplotJABBAres to show the combined seasonal data as boxplots
  * Fixup links to Cookbook sripts.
* Formatted DESCRIPTION URL and BugReports links to the PIFSCstockassessments repo too add to its R package site.

# ss3diags 2.0.0 

* ss3iags is now installed as an R-package. Package documentation is upatded to reflect these chenges. 
  * A simple, cod-like, Stock Synthesis model, simulated via ss3sim, replaces Pacific North Hake (`pac.hke`) and North Atlantic Shortfin Mako Shark (`natl.sma`) example datasets.
* SSplotRetro: fixed bug so that the shading in uncertainty area shows up when xlims are specified.
* SSplotJABBAres: added 'con' option to subplots argument so conditional age-at-length data can be plotted. Also added a seas argument so user can specify if data should be combined across seasons within a year or kept separate. 
* SScompsTA1.8: added a seas argument so users can specify if data should be combined across seasons within a year or kept separate.
* SSplot functions (SSplotModelComp, SSplotEnsemble, SSplotHCxval, SSplotJABBAres, SSplotRetro): Marked `plot`, `png`, `pdf`, `print`, and `new` as deprecated. They will be defunct in a future version.
* Added a `NEWS.md` file to track changes to the package.

# ss3diags 1.0.8

* Fixed MASE

# ss3diags 1.0.7

* Bug fixes

# ss3diags 1.0.6

* Improved SSdiagsMCMC

# ss3diags 1.0.5

* Updated SSdeltaMVLN

# ss3diags 1.0.4 

* Reference: authors changes 

# ss3diags 1.0.3

* Added annF_ quantaties

# ss3diags 1.0.2

* Added aut

# ss3diags 1.0.1

* Added SSplotH
