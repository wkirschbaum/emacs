x = 5

defmodule Foo.Bar do
  @moduledoc """
    Somecomment here
  """

  def reduce(function, acc, fun) when is_function(function, 2), do: function.(acc, fun)
  def foo, do: boo, else: boo

  foo = 1 + 3

  bar = fn foo ->
          bar
        end

  x = 5

  def foo() do
    x = %{
        foo: :bar
      }
  end

  "
  afooo
  "

  @moduledoc """
    Somecomment here

    def foolproof do
    bookBody
      end
  """
  defmodule Bar do
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
