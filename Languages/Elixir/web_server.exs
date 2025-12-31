# Hello, World!
IO.puts "Hello, World!"

defmodule WebServer do
  def make_http_request(url) do
    case HTTPoison.get(url, [], timeout: 5000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      _ ->
        {:error, "HTTP request failed"}
    end
  rescue
    _ -> {:error, "HTTP request failed"}
  end

  def test_web_server do
    base_url = "http://localhost:3000"
    compare_file = "../webserver/compare.json"
    output_file = "test-result.json"

    # Load expected hashes
    unless File.exists?(compare_file) do
      IO.puts "Error: Could not find #{compare_file}"
      exit(:normal)
    end

    {:ok, compare_content} = File.read(compare_file)
    expected_hashes = Poison.decode!(compare_content)

    IO.puts "Testing 100 endpoints..."

    results = []
    passed = 0
    failed = 0

    # Test each endpoint
    {results, passed, failed} = Enum.reduce(0..99, {results, passed, failed}, fn i, {acc_results, acc_passed, acc_failed} ->
      endpoint = "test-#{i}"
      url = "#{base_url}/#{endpoint}"

      result = %{
        endpoint: endpoint,
        url: url,
        expected_hash: expected_hashes[endpoint] || ""
      }

      case make_http_request(url) do
        {:ok, response} ->
          case Poison.decode(response) do
            {:ok, %{"hash" => server_hash}} ->
              updated_result = Map.put(result, :server_hash, server_hash)

              if server_hash == result.expected_hash do
                final_result = Map.put(updated_result, :status, "PASSED")
                {[final_result | acc_results], acc_passed + 1, acc_failed}
              else
                final_result = Map.put(updated_result, :status, "FAILED")
                {[final_result | acc_results], acc_passed, acc_failed + 1}
              end
            _ ->
              final_result = Map.put(result, :status, "FAILED") |> Map.put(:error, "JSON parse error")
              {[final_result | acc_results], acc_passed, acc_failed + 1}
          end
        {:error, error} ->
          final_result = Map.put(result, :status, "FAILED") |> Map.put(:error, error)
          {[final_result | acc_results], acc_passed, acc_failed + 1}
      end
    end)

    # Progress indicator
    Enum.each(1..10, fn i ->
      IO.puts "Tested #{i * 10}/100 endpoints..."
    end)

    # Create final result
    total_tests = length(results)
    success_rate = (passed / total_tests * 100) |> Float.round(1)

    final_result = %{
      total_tests: total_tests,
      passed: passed,
      failed: failed,
      success_rate: "#{success_rate}%",
      timestamp: DateTime.utc_now() |> DateTime.to_string(),
      results: Enum.reverse(results)
    }

    # Save results to file
    case File.write(output_file, Poison.encode!(final_result, pretty: true)) do
      :ok -> IO.puts "Results saved to: #{output_file}"
      {:error, reason} -> IO.puts "Error saving results: #{reason}"
    end

    IO.puts "\nTest completed!"
    IO.puts "Passed: #{passed}/#{total_tests} (#{success_rate}%)"
    IO.puts "Failed: #{failed}/#{total_tests} (#{100 - success_rate}%)"
  end
end

# Ensure HTTPoison and Poison are available
Application.ensure_all_started(:httpoison)

WebServer.test_web_server()
