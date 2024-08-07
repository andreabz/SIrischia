#' DT language options for italian language
#'
#' @description A list to be used inside of DT::datatable options
#'
#' @return A list
#'
#' @noRd
dt_italian <- list(
  length = "Mostra",
  emptyTable = "Nessun dato presente nella tabella",
  info = "Vista da _START_ a _END_ di _TOTAL_ elementi",
  infoEmpty ="Vista da 0 a 0 di 0 elementi",
  infoFiltered = "(filtrati da _MAX_ elementi totali)",
  infoPostfix = "",
  infoThousands = ".",
  lengthMenu = "Visualizza _MENU_ elementi",
  loadingRecords = "Caricamento...",
  processing = "Elaborazione...",
  search = "Cerca:",
  zeroRecords = "La ricerca non ha portato alcun risultato.",
  paginate = list(
    first = "Inizio",
    previous = "Precedente",
    `next` = "Successivo",
    last = "Fine"
  ),
  aria = list(
    sortAscending = ": attiva per ordinare la colonna in ordine crescente",
    sortDescending = ": attiva per ordinare la colonna in ordine decrescente"
  )
)
