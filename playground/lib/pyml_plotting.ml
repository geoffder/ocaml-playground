open Base
open Pyops

let () = Py.initialize ()

let plt = Py.import "matplotlib.pyplot"

let py_floats_of_list =
  Array.of_list
  |> Fn.compose @@ Py.Array.of_array Py.Float.of_float Py.Float.to_float

let plt_plot_kw =
  Py.Module.get_function_with_keywords plt "plot"

let plot_kw data_lists kw =
  let args = List.(data_lists |> map ~f:py_floats_of_list |> to_array) in
  plt_plot_kw args kw |> ignore

let plt_scatter_kw =
  Py.Module.get_function_with_keywords plt "scatter"

let scatter data_lists kw =
  let args = List.(data_lists |> map ~f:py_floats_of_list |> to_array) in
  plt_scatter_kw args kw |> ignore

let show () = plt.&("show") [||]
