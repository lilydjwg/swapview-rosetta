#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <tuple>
#include <vector>

#ifdef PARALLEL
#include <execution>
// Writing the currying thing takes a lot of time:
// http://cpptruths.blogspot.com/2018/12/simple-template-currying.html
#define sortx(...) std::sort(std::execution::par_unseq, __VA_ARGS__)
#define transformx(...) std::transform(std::execution::par_unseq, __VA_ARGS__)
#define remove_ifx(...)  std::remove_if(std::execution::par_unseq, __VA_ARGS__)
#else
#define sortx(...) std::sort(__VA_ARGS__)
#define transformx(...) std::transform(__VA_ARGS__)
#define remove_ifx(...)  std::remove_if(__VA_ARGS__)
#endif /* PARALLEL */

using swap_info = std::tuple<int, double, std::string>;

auto starts_with(const std::string &self, const std::string &prefix) -> bool {
  return self.length() >= prefix.length() &&
         std::equal(prefix.begin(), prefix.end(), self.begin());
}

auto filesize(double size) -> std::string {
  constexpr char units[]{"KMGT"};
  int unit = -1;
  for (; size > 1100 && unit < 3; ++unit) size /= 1024;
  if (unit == -1)
    return std::to_string(static_cast<size_t>(size)) + 'B';
  std::ostringstream oss;
  oss << std::fixed << std::setprecision(1)
      << size << units[unit] << "iB";
  return oss.str();
}

auto get_comm_for(const std::filesystem::path &p) -> std::string {
  std::ifstream file{p};
  std::string buf{std::istreambuf_iterator{file}, {}};
  if (!buf.empty() && buf.back() == '\0') buf.pop_back();
  std::replace(buf.begin(), buf.end(), '\0', ' ');
  return buf;
}

auto get_swap_for(const std::filesystem::path &p) -> size_t {
  constexpr char TARGET[]{"Swap:"};
  std::ifstream file{p};
  std::string buf;
  size_t s = 0;
  while (getline(file, buf))
    if (starts_with(buf, TARGET))
      s += std::strtol(buf.c_str() + std::size(TARGET), nullptr, 10);
  return 1024 * s;
}

#ifdef LAMBDA
auto get_swap() -> std::vector<swap_info> {
  std::vector<swap_info> result;
  auto proc = std::filesystem::directory_iterator{"/proc"};
  transformx(proc.begin(), proc.end(), result.begin(), [](const auto& entry) {
    if (int pid = std::strtol(entry.path().filename().c_str(), nullptr, 10))
      if (size_t swp = get_swap_for(entry.path() / "smaps"))
        return {pid, swp, get_comm_for(entry.path() / "cmdline")};
    return {0, 0.0, ""};
  });
  result.erase(remove_ifx(result.begin(), result.end(), [](const auto& maybe_res) {
    return std::get<0>(maybe_res) == 0;
  }));
  sortx(result.begin(), result.end(), [](const auto &lhs, const auto &rhs) {
    return std::get<1>(lhs) < std::get<1>(rhs);
  }); 
  return result;
}
#else
auto get_swap() -> std::vector<swap_info> {
  std::vector<swap_info> result;
  for (const auto &entry: std::filesystem::directory_iterator{"/proc"})
    if (int pid = std::strtol(entry.path().filename().c_str(), nullptr, 10))
      if (size_t swp = get_swap_for(entry.path() / "smaps"))
        result.emplace_back(pid, swp, get_comm_for(entry.path() / "cmdline"));
  std::sort(result.begin(), result.end(), [](const auto &lhs, const auto &rhs) {
    return std::get<1>(lhs) < std::get<1>(rhs);
  });
  return result;
}
#endif /* LAMBDA */

int main() {
  std::ios::sync_with_stdio(false);
  std::cout << std::setw(7) << "PID" << ' '
            << std::setw(9) << "SWAP" << ' '
            << std::setw(0) << "COMMAND" << '\n';
  size_t total = 0;
  for (const auto &[pid, swp, cmd]: get_swap()) {
    std::cout << std::setw(7) << pid << ' '
              << std::setw(9) << filesize(swp) << ' '
              << std::setw(0) << cmd << '\n';
    total += swp;
  }
  std::cout << "Total:" << std::setw(9) << filesize(total) << std::endl;
}
