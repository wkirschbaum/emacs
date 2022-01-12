defp status(kyc_status) do
  case kyc_status do
    a ->
      b
      asdf
      sdf
      fn foo ->
        moo
      end

    c ->
      d

    k ->
      d
  end

  case kyc_status do
    %KycStatus{status: status} -> status

    %KycStatus{status: status} -> status

    _ -> :foo
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
