#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
  // load the data
  ifstream data_file("path/to/german_data_clean.csv");
  string line;
  vector<vector<string>> data;
  while (getline(data_file, line)) {
    vector<string> row;
    size_t pos = 0;
    string token;
    while ((pos = line.find(",")) != string::npos) {
      token = line.substr(0, pos);
      row.push_back(token);
      line.erase(0, pos + 1);
    }
    row.push_back(line);
    data.push_back(row);
  }
  
  // modify the data set
  vector<string> selected_purposes = {"car (new)", "radio/television", "furniture/equipment"};
  
  vector<vector<string>> subset_1;
  for (vector<string> row : data) {
    if (row[4] == "male   : divorced/separated" &&
        stoi(row[1]) >= 30 &&
        find(selected_purposes.begin(), selected_purposes.end(), row[3]) != selected_purposes.end()) {
      vector<string> new_row = {row[1], row[2], row[5], row[3], row[7]};
      subset_1.push_back(new_row);
    }
  }
  
  sort(subset_1.begin(), subset_1.end(), [](const vector<string>& row1, const vector<string>& row2) {
    return stoi(row1[2]) > stoi(row2[2]);
  });
  
  // write the subset to a CSV file
  ofstream output_file("my_subset_cpp.csv");
  for (vector<string> row : subset_1) {
    for (size_t i = 0; i < row.size(); ++i) {
      output_file << row[i];
      if (i != row.size() - 1) {
        output_file << ";";
      }
    }
    output_file << "\n";
  }
  
  return 0;
}

