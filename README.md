<h1 align="center"> Covid_19 Rates Package </h1>

The package has a Shiny app to explore the Covid-19 outbreak in Italy.

## Lavora su
Andrea: try run  python code on  R  <br>
Fabio: lavoro su analysis.R <br>
Federico: <br>
Gregorio: <br>
Marzio: <br>


# Contributors :busts_in_silhouette:
- Andrea Ierardi   &nbsp; <a href="https://www.linkedin.com/in/andreaierardi/" rel="nofollow noreferrer">
    <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn
  </a>  &nbsp;
  <a href="https://github.com/Andreaierardi" rel="nofollow noreferrer"> <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; 
  <a href="https://ierardiandrea.com/" rel="nofollow noreferrer">  <img src="https://github.com/Andreaierardi/Master-DataScience-Notes/blob/master/img/website.svg" width=15>  Website </a>
  
- Marzio De Corato &nbsp; <a href="https://www.linkedin.com/in/marzio-de-corato-2351a44b/" rel="nofollow noreferrer">
    <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn
  </a>  &nbsp;
  <a href="https://github.com/marzione00" rel="nofollow noreferrer"> <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; 
   
- Gregorio Saporito &nbsp; <a href="https://www.linkedin.com/in/greg-saporito/" rel="nofollow noreferrer">
    <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn
  </a>  &nbsp;
  <a href="https://github.com/gregorio-saporito" rel="nofollow noreferrer"> <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; 

  
- Fabio Caironi &nbsp; <a href="https://www.linkedin.com/in/fabio-caironi-8361091a2/" rel="nofollow noreferrer">
    <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn
  </a>  &nbsp;
  <a href="https://github.com/fabio130497" rel="nofollow noreferrer"> <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; 
 
  
- Federico Matteucci &nbsp; 
  <a href="https://github.com/De-Rham-Cohomology" rel="nofollow noreferrer"> <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; 


## Installation

```R
# First install the R package "devtools" if not installed
devtools::install_github('marzione00/COVID_19_HACK')
```

## Usage

Load the package

```R
require(covid19)
```

The function of the package is `runC19()` and is run without arguments.

```R
runC19()
```

## Folder structure
```
covid19 
├── R
|   ├── get_countryTS.R
|   ├── get_provTS.R
|   ├── get_regionTS.R
|   └── runC19.R
|
├── inst
│   └── shiny
|         └── C19
│              ├── server
│              |     ├── 1module
|              |     |      └── map.R
│              |     ├── 2module
│              |     ├── 3module
│              |     ├── 4module
│              │     └── global
│              │            ├── shinyjs.R
│              │            └── sidebar.R
│              │   
│              ├── ui
│              |    ├── global
│              |    │     └── css.R
│              |    │       
│              |    └── tabs
│              |          ├── 1.tab_home.R
│              |          ├── 2.tab_inspection.R
│              |          ├── 3.tab_analysis.R
|              |          └── 4.tab_conclusion.R
|              ├── server.R
|              └── ui.R
├── man
│    ├── get_countryTS.Rd
│    ├── get_provTS.Rd
│    ├── get_regionTS.Rd
│    └── runC19.Rd
├── .gitignore
├── LICENSE
├── .Rbuildignore
├── .Covid_19.Rproj
├── DESCRIPTION
├── NAMESPACE
└── README.md
```
