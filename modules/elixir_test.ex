def all?(enumerable) do
  Enumerable.reduce(enumerable, {:cont, true}, fn entry, _ ->
                                                 if entry, do: {:cont, true}, else: {:halt, false}
                                                 end)
  |> elem(1)
end

foo

{a, b, c, d, e, g, h, p, k, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t} =
  9

def foo do
  fsdafd
end

@type foo ::
  String.t()


    @type result ::
    {:done, term}
      | {:halted, term}
      | {:suspended, term, continuation}

      4 = 5

      x =
        5 +
          5 -
          6 * 4

      5
      |> time
      |> IO.puts()
