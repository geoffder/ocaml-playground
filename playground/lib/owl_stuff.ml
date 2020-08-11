(* Example of how to slice Owl matrices:
 *  See: https://www.cl.cam.ac.uk/~lw525/owl/chapter/slicing.html
 *
 *  Mat.get_fancy [ R[ 0; (List.length rig.time) - 1]; R[] ] matrix
 *  Each element corresponds to a dimension. R is a constructor for ranges,
 *  formatted like so [ start; stop; step ]. Empty range = take whole dim.
 * *)
