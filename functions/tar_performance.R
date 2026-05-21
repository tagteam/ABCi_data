tar_performance <- function(print=TRUE){
    m = tar_meta()
    data.table::setDT(m)
    m = m[type == "stem"]
    m[,Memory := utils:::format.object_size(bytes,units="GB")]
    m[,Runtime := format(lubridate::as.duration(seconds),units = "auto")]
    m = m[,.(Target = name,Runtime,Memory,Storage = format)]
    if (print) print(m)
    invisible(m)
}
