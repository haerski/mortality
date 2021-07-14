# mortality
Tracking mortality during a pandemic. Data repository can be found [here](https://drive.google.com/drive/folders/1KXtlXRMk_WL9dHToFAGNoC2vageeNmja?usp=sharing).

## General workflow
1. Click on "Fetch" in order to get the latest version of the repository from the website to your computer
1. Merge master into your branch to get the latest version on your branch
1. Start working on your computer.
1. When you're happy with the chunk of code you've written, commit those changes to your branch
1. When you're happy with the state of your branch (i.e. nothing is broken, you've implemented some feature/model/plot/analysis), merge your branch into master so that everyone can enjoy your work
1. Don't forget to click on Push every so often!!! This backs ups your work on the GitHub website and makes your changes visible to all of us!

## Guidelines
* Please work on your own branch (not `master`)
* Don't upload datasets here, use the data repository (https://drive.google.com/drive/folders/1KXtlXRMk_WL9dHToFAGNoC2vageeNmja?usp=sharing). The data files are assumed to reside in the `data` directory.
* Please keep the master branch relatively clean. If you want to experiment, you can create new branches and merge them to your branch (or delete them if the experiment failed)
* Please **fetch** and merge `master` to your own branch often. You don't want to be working on old versions of the code
* There might be cases when pushing will fail due to reasons. If that's the case, please create a **Pull Request** on the GitHub website, Marc will try to fix it.
* Don't include any data files to your commits. I've made git ignore `.csv` files, so this shouldn't happen. Let me know if you find data files with other extensions.
* Try not to include any non-text data in the repository (this includes e.g. Jupyter notebooks, although there is a way to make them play nicely with git)
