defp status(kyc_status) do
  case kyc_status do
    a -> b
    c -> d
        k -> d
  end

  case kyc_status do
    %KycStatus{status: :failed} -> :not_started
    %KycStatus{status: status} -> status
                                  _ -> :not_started
  end
end
