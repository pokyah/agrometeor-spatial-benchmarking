# Spatialisation with R

## Using the Agromet API
In order to use the Agromet API, you will need your own API token. You can get it at https://app.pameseb.be.

Once you have your key, add it as an environment variable in your project's `.Renviron` file under the name `AGROMET_API_V1_KEY=<pasteyourkeyhere>`

The code will access to your API key using `Sys.getenv("AGROMET_API_V1_KEY")`.

To know more about R startup configuration and environmental, check the freely online accessible Efficient R programming [book](https://csgillespie.github.io/efficientR/3-3-r-startup.html#fnref4)
