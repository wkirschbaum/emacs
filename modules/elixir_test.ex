try do
  foo = bar
catch
  _ ->
    foo

    fn
      foo ->
        fo

      _ ->
        boo
    end

  false ->
    another
end

@foo "bar"

defmodule Foo do
  def foo do
    bar
  end

  defmodule Boo do
    def ppp() do

    end

    defmodule Zoo do
      def boo do
        x = if true do
              foo
            else
              boo
            end
      else
        x = if true do
              foo
            else
              boo
            end
      end
    end
  end
end

def foo, do: boo, else: boo

def all?(enumerable) do
  Enumerable.reduce(enumerable, {:cont, true}, fn entry, _ ->
                                                 if entry, do: {:cont, true}, else: {:halt, false}
                                               end)
  |> elem(1)
end

defp zip_with_list([head1 | next1], [head2 | next2], fun) do
  [
    fun.(head1, head2) | zip_with_list(next1, next2, fun)
  ]
end

@moduledoc """
Somecomment here
"""
defmodule Foo.Bar do
  # def reduce(function, acc, fun) when is_function(function, 2), do: function.(acc, fun)
  def foo, do: boo, else: boo

  foo =
    1 + 3

  bar = fn foo ->
          bar
        end

  def foo() do
    x = %{
        foo: :bar
      }
  end
end

@doc """
Hello world
def foo
bar
end
"""
# foo bar
defp status(kyc_status) do
  case kyc_status do
    a ->
      b
      asdf
      "foo #{foo = bar}"

      x = [
        "one",
        "two"
      ]

      sdf

      fn foo ->
        moo
      end

    c ->
      d

    k ->
      d
  end

  # foo bar
  case kyc_status do
    %KycStatus{status: status} ->
      status

    %KycStatus{status: status} ->
      status

    _ ->
      :foo

    _ ->
      :not_started

    _ ->
      :not_started

    _ ->
      :not_started
  end

  case kyc_status do
    %KycStatus{status: :failed} -> :not_started
    %KycStatus{status: status} -> status
    _ -> :foo
    _ -> :not_started
    _ -> :not_started
    _ -> :not_started
  end

  case kyc_status do
    %KycStatus{status: status} ->
      status

    _ ->
      :foo

    _ ->
      :not_started

    _ ->
      :not_started

    _ ->
      :not_started
  end
end
