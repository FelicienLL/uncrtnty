# uncrtnty 0.1.2
* uncrtnty-list object are created thanks to `uncrtnty()`, that combines a constructor and a validator for more safety (see #3).
* `compute_df()` now accepts a `maxdf` to censor the maximum degree of freedom to the number of id/observations (see #2)
* Add `parse_lst()`, that can also extract the number of ID/OBS from the .lst, instead of `parse_blockform_from_lst()`
* Remove the `get_x()` functions that extracted information from xpose_data base objects
* Update README and examples

# uncrtnty 0.1.1
* The package now works on a single `uncrtnty`-list object, that is 
  - created "from": xpose data base, NONMEM outputs, etc...
  - transformed "to" : code for NONMEM PRIOR, arguments for simpar etc...
* Add `u_from_xpdb()`, `u_to_nwpri()`, `u_to_simpar()`
* Remove `xpdb_to_simpar()`
* Changes in readme
* Possibility to work on *.phi* files (`parse_phi()` and `get_phi()`)
* Add a .xml file
* Bug fix : `low_to_matrix` fill the lower matrix properly.

# uncrtnty 0.1.0.9001

* Added a `NEWS.md` file to track changes to the package.
