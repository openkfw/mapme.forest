**Tutorial**

The tutorial for the usage of the **mapme.forest package** can be found [here](https://tutorials-mapme.test.sixmarkets.net/). You can find installation
instructions below.


**Usage**

The installation and usage of this package depends on the SDMTools package which currently no longer is hosted on CRAN. However, it can be installed from Rforge using this command:

`install.packages('SDMTools', repos = 'http://rforge.net/', source = TRUE)`

Alternativley, it can also be installed from GitHub using:

`devtools::install_github("jjvanderwal/SDMTools")`

It is the GitHub version of the package which is going to be installed when the SDMTools dependency is not met on a machine during installation of
the ForestIndicator packages.

The ForestIndicators package can then be installed by using the `devtools` package by entering the following command:

`devtools::install_github("openkfw/mapme.forest")`.

Additionally, we ship this package with a Dockerfile which can be used to run the packages and its dependencies as a container. When `cd`ing into the repository,  building and running the image is as simple as:

`docker build -t mapmeForest:latest .`

`docker run -d -p 8787:8787 -e USER=myuser -e PASSWORD=mypassword gans-deforestation`

**Installation instructions under Windows**

In our installation instruction we assume that you are going to use R Studio as an IDE. You need to have some software pre-installed software to be able to install the forestIndicator package.

- download and install R >= 3.5.x from https://cran.rstudio.com/
- download and install R Studio from https://rstudio.com/products/rstudio/download/#download
- download and install Rtools from https://cran.r-project.org/bin/windows/Rtools and make Rtools available on the PATH variable
- install the devtools and git2r packages with 'install.packages(c("devtools", "git2r"))'
- install forestIndicators with the following command replacing <user> and <pass> with your username and password: `devtools::install_git("https://innersource.stackato.de/gans/deforestation", credentials = git2r::cred_user_pass("<user>", "<pass>")` and close R Studio afterwards

Now the package has been sucessfully installed, however, you are not necessarily 100% ready to use it. Since some of the function use gdal system calls we additionally need a valid gdal installation on your machine. 
- go to https://trac.osgeo.org/osgeo4w/ and download the osgeo4w.exe and run it
- chose advanced installation and follow the instructions. In the follwing we assume you choose the standard installation path. If you chose a custom installation path adjust the next inputs accordingly.
- When you are asked which software to install open up the Commandline_Utilities branch and chose the following:
	- gdal 
	- all python3-* entries
- At the Libs branch chose the following package:
	- libs python3-gdal

Once the installation is completed you have to add some environment variables so that Windows know where to look for the programs.
- open: System Controls -> System and Safety -> System -> Advanced Settings -> Environment Variables -> System Variables
- edit the PATH variable and add these three entries:
	- C:\OSGeo4W64\bin
	- C:\OSGeo4W64\apps\Python37
	- C:\OSGeo4W64\apps\Python37\Scripts
- then add four new variables with the following scheme:
	- Name: GDAL_DATA Value: C:\OSGeo4W64\share\gdal
	- Name: PROJ_LIB Value: C:\OSGeo4W64\share\proj
	- PYTHONHOME Value: C:\OSGeo4W64\apps\Python37
	- PYTHONPATH Value: C:\OSGeo4W64\apps\Python37\python.exe
- open up a command line and enter: gdal_calc.py When you are getting some information on how to use this script you are ready to go
- note that the setup outlined above might not be your preffered setup. Of course you can use different setups, e.g. by using virtual anaconda environments. However, this would be fairly advanced setups so we do not outline them here. The important take-away message four you when you are going to use a different setup is that gdal_calc.py as well as the usual gdal command line tools need to be available and executable.
