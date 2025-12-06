#include <iostream>
#include <string>
using namespace std;
bool repetition(string s)
{
    for (int i = 1; i < s.length(); i++)
    {
        if (s.substr(0, i) + s.substr(i, s.length()) == s.substr(i, s.length()) + s.substr(0, i))
        {
            // cout << s.substr(0, i) << " " << s.substr(i, s.length()) << " == " << s.substr(i, s.length()) << " " << s.substr(0, i) << endl;
            return true;
        }
    }
    return false;
}

int main()
{
    string x;
    cin >> x;
    x = x + ",";
    unsigned long long int sum = 0;
    while (!x.empty())
    {
        int start = 0;
        int end;
        end = x.find("-") + 1;
        unsigned long long int range_start = stoull(x.substr(start, end - 1).c_str());
        x = x.substr(end);
        end = x.find(",") + 1;
        unsigned long long int range_end = stoull(x.substr(start, end - 1).c_str());
        x = x.substr(end);
        for (unsigned long long int i = range_start; i <= range_end; i++)
        {
            string s = to_string(i);
            if (repetition(s))
            {
                cout << sum << " " << i << endl;
                sum = sum + i;
            }
        }
    }
    cout << sum << endl;
}
