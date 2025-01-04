method find_min(a: array?<int>) returns (m: int)
  requires a == null || a.Length > 0  // Precondition: The array is either null or has at least one element.
  ensures a == null ==> m == 0        // Postcondition: If the array is null, return 0.
  ensures a != null ==> (forall i :: 0 <= i < a.Length ==> m <= a[i]) 
  ensures a != null ==> (exists i :: 0 <= i < a.Length && m == a[i]) 
{
  if a == null {
    m := 0; // If the array is null, return 0.
    return;
  }
  var min := a[0];
  var i := 1;
  while i < a.Length
    invariant 0 <= i <= a.Length                 
    invariant (forall j :: 0 <= j < i ==> min <= a[j]) 
    invariant (exists j :: 0 <= j < i && min == a[j]) 
  {
    if a[i] < min {
      min := a[i]; 
    }
    i := i + 1; // Move to the next element.
  }

  m := min; 
}
