% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pdxTrees_streets.R
\name{get_pdxTrees_streets}
\alias{get_pdxTrees_streets}
\title{Load the pdxTrees_streets data}
\source{
\url{https://www.portlandoregon.gov/parks/article/433143}
}
\usage{
get_pdxTrees_streets(neighborhood = NULL)
}
\arguments{
\item{neighborhood}{A vector of neighborhood names for filtering the data. 
If NULL, all street trees will be returned.}
}
\value{
Returns a data frame with 218602 rows and 23 variables:
\describe{
  \item{UserID}{ID}
  \item{Inventory_Date}{Date of data collection}
  \item{Species}{Species of the tree.  All dead trees were listed as "unknown"}
  \item{Common_Name}{Common name of the tree}
  \item{DBH}{Diameter at breast height (4.5' above ground)}
  \item{Condition}{Trees were rated as good, fair, poor, or dead. 
  These general ratings reflect whether or not a tree is likely to
   continue contributing to the urban forest (good and fair trees)
    or whether the tree is at or near the end of its life (poor and dead trees).}
  \item{Site_Type}{Where along the street the tree was located.
  There are 8 different site types and more info can be found here:
   \url{http://gis-pdx.opendata.arcgis.com/datasets/street-trees}}
  \item{Site_Size}{Categorical size of the site: Small, Medium, Large}
  \item{Site_Width}{How wide the site was in ft.}
  \item{Wires}{Whether or not the site had wires: High voltage,
   No High voltage (No HV), other}
  \item{Site_Development}{ The condition of the site either being improved
   (ex. along a side walk or paved roadway ) or unimproved (a gravel road))}
  \item{Address}{The address where the tree is located}
  \item{Neighborhood}{The Portland neighborhood in which the tree is located}
  \item{Collected_By}{Who collected the data on this tree: staff or volunteer}
  \item{Scientific}{Scientific name of the tree}
  \item{Family}{Family of the tree}
  \item{Genus}{Genus of the tree}
  \item{Functional_Type}{Categorical variable with groups: 
  Broadleaf Deciduous (BD), Broadleaf Evergreen (BE),
   Coniferous Deciduous (CD), and Coniferous Evergreen (CE)}
  \item{Mature_Size}{Categorical variable with groups: Large (L),
   Medium (M), and Small (S).  Categorization is based on  the height,
   canopy width, and general form of the tree at maturity}
  \item{Edible}{Categorical variable of edible trees}
  \item{Notes}{Note on the data collection}
  \item{Longitude}{Longitude}
  \item{Latitude}{Latitude}
}
}
\description{
This function pulls the pdxTrees_streets dataset from the GitHub repository:
\url{https://github.com/mcconvil/pdxTrees}. pdxTrees_streets is a data frame of
 all the street trees in the 96 neighborhoods of Portland, OR 
 and was collected as part of the Urban Forestry Tree Inventory Project.
}
\examples{
 # To grab all trees
\donttest{get_pdxTrees_streets()} 

# To grab trees from one neighborhood
\donttest{get_pdxTrees_streets(neighborhood = "Concordia")}

# To grab trees from multiple neighborhoods 
\donttest{get_pdxTrees_streets(neighborhood = c("Concordia","Eastmoreland","Sunnyside"))} 

}
