 let rec fibonacci n =
  if n <= 0 then
    invalid_arg "n must be a positive integer"
  else if n = 1 || n = 2 then 1
  else
    fibonacci(n - 1) + fibonacci(n - 2)