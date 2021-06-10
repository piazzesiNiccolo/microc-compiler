char b[13] = "ciao mondo\n";
int main() {
    char c[] = "hello,world\n";
    int i;
    for(i = 0; c[i] != '\0'; i++){
        print_char(c[i]);
    }
    for(i = 0; b[i] != '\0'; i++){
        print_char(b[i]);
    }
    return 0;
}
