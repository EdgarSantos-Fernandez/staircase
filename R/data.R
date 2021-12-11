#' Over 50,000 classifications of over 3,500 images
#'
#' @format A data frame with 55223 observations of 31 variables:
#'  \describe{
#'    \item{annotationID}{The ID number of the annotation}
#'    \item{originalImageID}{The ID of the classification (there are many elicitations in each image)}
#'    \item{annotationCategoryID}{The category that the annotation falls under}
#'    \item{hashedUserID}{The ID of the user classifying the image}
#'    \item{MediaId}{numeric image identifier (de-identified)}
#'    \item{lon}{Longitude of the image location}
#'    \item{lat}{Latitude of the image location}
#'    \item{year}{The year that the image was uploaded}
#'    \item{Source}{The uploader of the image}
#'    \item{IsUploadedImage}{A binary variable indicating whether the image was uploaded by a user}
#'    \item{Date}{The date that the classification was made on}
#'    \item{cat}{The classification made by the user for this elicitation}
#'    \item{Y}{Vertical location of the elicitation point centre (in pixels)}
#'    \item{X}{Horizontal location of the elicitation point centre (in pixels)}
#'    \item{Classification}{Correct classification for that respective elicitation}
#'    \item{width}{The width of the image (in pixels)}
#'    \item{height}{The height of the image (in pixels)}
#'    \item{True_Species}{Classification made by the user}
#'    \item{hard_bin_true}{A binary variable indicating if the user successfully identified an elicitation containing hard coral}
#'    \item{hard_bin}{Was the elicitation classifying hard coral}
#'    \item{value}{Binary variable indicating if the elicitation was hard coral and if the user successfully classified it as hard coral}
#'    \item{siteID}{Image ID number}
#'    \item{userID}{User ID number}
#'    \item{correct}{A binary variable indicating if the users' classification is correct}
#'    }
"reefdiverdata"
