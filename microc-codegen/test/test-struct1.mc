struct Node {
    int v;
    struct Node* left;
    struct Node* right;
};
void print_tree_in_dfs(struct Node *root){
    if(root != NULL){
        print_tree_in_dfs((*root).left);
        print((*root).v);
        print_tree_in_dfs((*root).right);
        
    }
}
int main(){
    struct Node l1;
    l1.left = NULL;
    l1.right = NULL;
    l1.v = 2;
    struct Node l2;
    l2.left = NULL;
    l2.right = NULL;
    l2.v = 3;
    struct Node root;
    root.v = 1;
    root.left = &l1;
    root.right = &l2;
    print_tree_in_dfs(&root);
    return 0;
}