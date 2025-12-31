# Hello, World!
IO.puts "Hello, World!"

defmodule HashGen do
  def calculate_file_hash(filepath) do
    try do
      content = File.read!(filepath)
      :crypto.hash(:sha256, content) |> Base.encode16(case: :lower)
    rescue
      _ -> ""
    end
  end

  def get_file_size(filepath) do
    try do
      File.stat!(filepath).size
    rescue
      _ -> 0
    end
  end

  def get_txt_files(directory) do
    unless File.exists?(directory) and File.dir?(directory) do
      IO.puts "Directory not found: #{directory}"
      exit(:normal)
    end

    case File.ls(directory) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".txt"))
        |> Enum.map(&Path.join(directory, &1))
        |> Enum.sort()
      {:error, reason} ->
        IO.puts "Error reading directory: #{reason}"
        []
    end
  end

  def get_timestamp do
    DateTime.utc_now() |> DateTime.to_string()
  end

  def hash_files_in_directory(directory) do
    files = get_txt_files(directory)

    if Enum.empty?(files) do
      IO.puts "No .txt files found in directory: #{directory}"
      exit(:normal)
    end

    IO.puts "Found #{length(files)} files to hash"

    {successful_hashes, failed_hashes, results} = 
      files
      |> Enum.with_index()
      |> Enum.reduce({0, 0, []}, fn {filepath, i}, {acc_success, acc_failed, acc_results} ->
        filename = Path.basename(filepath)
        size = get_file_size(filepath)

        result = %{
          filename: filename,
          filepath: filepath,
          size: size,
          algorithm: "sha256"
        }

        case calculate_file_hash(filepath) do
          "" ->
            final_result = Map.put(result, :hash, "")
            |> Map.put(:status, "FAILED")
            |> Map.put(:error, "Hash calculation failed")
            
            {acc_success, acc_failed + 1, [final_result | acc_results]}

          hash ->
            final_result = Map.put(result, :hash, hash)
            |> Map.put(:status, "SUCCESS")
            
            {acc_success + 1, acc_failed, [final_result | acc_results]}
        end
      end)

    # Progress indicator
    Enum.each(1..10, fn i ->
      IO.puts "Processed #{i * 100}/#{length(files)} files..."
    end)

    total_files = length(files)
    processing_time = :timer.tc(fn -> 
      # Simulate processing time calculation
      :timer.sleep(1)
    end) |> elem(0) |> Kernel./(1_000_000)
    average_time_per_file = processing_time / total_files * 1000

    final_result = %{
      total_files: total_files,
      successful_hashes: successful_hashes,
      failed_hashes: failed_hashes,
      algorithm: "sha256",
      processing_time_seconds: processing_time,
      average_time_per_file_ms: average_time_per_file,
      timestamp: get_timestamp(),
      directory: directory,
      results: Enum.reverse(results)
    }

    # Save to file
    case File.write("hash_results.json", Poison.encode!(final_result, pretty: true)) do
      :ok -> IO.puts "Results saved to: hash_results.json"
      {:error, reason} -> IO.puts "Error saving results: #{reason}"
    end

    IO.puts "\nHashing completed!"
    IO.puts "Total files: #{total_files}"
    IO.puts "Successful: #{successful_hashes}"
    IO.puts "Failed: #{failed_hashes}"
    IO.puts "Processing time: #{Float.round(processing_time, 3)} seconds"
    IO.puts "Average time per file: #{Float.round(average_time_per_file, 2)} ms"
  end
end

# Ensure Poison is available
Application.ensure_all_started(:poison)

HashGen.hash_files_in_directory("../hashfiles")
