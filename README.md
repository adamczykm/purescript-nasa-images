# purescript-nasa-api

implements image api of Nasa images based on 
[this document](https://images.nasa.gov/docs/images.nasa.gov_api_docs.pdf)

 - `Methods.search` runs a search for specified request and returns a `Validation` of search result
 - `Methods.retrieve` retrieves asset based on URL which is available in search item
 - `Methods.searchAndRetrieve` combines above functions, providing validation which returns
    search results with filled asset information

Example usage in `test/Main.purs`

## Install

You have to install `xhr2` or just run npm install.
