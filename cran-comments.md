## Test environments

* local Windows install, R 4.2.1
* Ubuntu Linux 20.04.1 LTS (on R-hub), R 4.2.0
* Fedora Linux (on R-hub) R-devel
* Windows (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. 

There is one NOTE that only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

---

This version includes 1 bug fix. 
