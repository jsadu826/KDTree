#include <algorithm>
#include <cassert>
#include <iostream>
#include <stdexcept>
#include <tuple>
#include <vector>

template <typename...> class KDTree;

/**
 * @typedef Key         key type
 * @typedef Value       value type
 * @typedef Data        key-value pair
 * @static  KeySize     k (number of dimensions)
 */
template <typename ValueType, typename... KeyTypes>
class KDTree<std::tuple<KeyTypes...>, ValueType> {
public:
  typedef std::tuple<KeyTypes...> Key;
  typedef ValueType Value;
  typedef std::pair<const Key, Value> Data;
  static inline constexpr size_t KeySize = std::tuple_size<Key>::value;
  static_assert(KeySize > 0, "Can not construct KDTree with zero dimension");

protected:
  struct Node {
    Data data;
    Node *parent;
    Node *left = nullptr;
    Node *right = nullptr;

    Node(const Key &key, const Value &value, Node *parent)
        : data(key, value), parent(parent) {}

    const Key &key() { return data.first; }

    Value &value() { return data.second; }
  };

public:
  // A bi-directional iterator for the KDTree
  class Iterator {
  private:
    KDTree *tree;
    Node *node;

    Iterator(KDTree *tree, Node *node) : tree(tree), node(node) {}

    // Increment the iterator
    void increment() {
      if (!node)
        return;
      if (node->right) // Find left most in the right sub tree
      {
        auto tmp = node->right;
        while (tmp->left)
          tmp = tmp->left;
        node = tmp;
        return;
      }
      auto tmp = node;
      while (tmp->parent) // Find the first valid ancestor
      {
        if (tmp->parent->left && tmp->parent->left == tmp) {
          node = tmp->parent;
          return;
        }
        tmp = tmp->parent;
      }
      node = nullptr;
      return;
    }

    // Decrement the iterator
    void decrement() {
      if (!node) // Find right most
      {
        if (!tree->root)
          return;
        auto tmp = tree->root;
        while (tmp->right)
          tmp = tmp->right;
        node = tmp;
        return;
      }
      if (node->left) // Find right most in the left sub tree
      {
        auto tmp = node->left;
        while (tmp->right)
          tmp = tmp->right;
        node = tmp;
        return;
      }
      auto tmp = node;
      while (tmp->parent) // Find the first ancestor
      {
        if (tmp->parent->right && tmp->parent->right == tmp) {
          node = tmp->parent;
          return;
        }
        tmp = tmp->parent;
      }
      return;
    }

  public:
    friend class KDTree;

    Iterator() = delete;

    Iterator(const Iterator &) = default;

    Iterator &operator=(const Iterator &) = default;

    Iterator &operator++() {
      increment();
      return *this;
    }

    Iterator operator++(int) {
      Iterator temp = *this;
      increment();
      return temp;
    }

    Iterator &operator--() {
      decrement();
      return *this;
    }

    Iterator operator--(int) {
      Iterator temp = *this;
      decrement();
      return temp;
    }

    bool operator==(const Iterator &that) const { return node == that.node; }

    bool operator!=(const Iterator &that) const { return node != that.node; }

    Data *operator->() { return &(node->data); }

    Data &operator*() { return node->data; }
  };

protected:
  Node *root = nullptr; // root of the tree
  size_t treeSize = 0;  // size of the tree

  /**
   * Find the node with key
   * @tparam DIM current dimension of node
   * @param key
   * @param node
   * @return the node with key, or nullptr if not found
   */
  template <size_t DIM> Node *find(const Key &key, Node *node) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (!node)
      return nullptr;
    if (key == node->data.first)
      return node;
    if (compareKey<DIM, std::less<>>(key, node->data.first))
      return find<DIM_NEXT>(key, node->left); // Recurse on left sub tree
    else
      return find<DIM_NEXT>(key, node->right); // Recurse on right sub tree
  }

  /**
   * Insert the key-value pair, if the key already exists, replace the value
   * @tparam DIM current dimension of node
   * @param key
   * @param value
   * @param node
   * @param parent
   * @return whether insertion took place (return false if the key already
   * exists)
   */
  template <size_t DIM>
  bool insert(const Key &key, const Value &value, Node *&node, Node *parent) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (!node) {
      node = new Node(key, value, parent);
      ++treeSize;
      return true;
    }
    if (key == node->data.first) {
      node->data.second = value;
      return false;
    }
    if (compareKey<DIM, std::less<>>(key, node->data.first))
      return insert<DIM_NEXT>(key, value, node->left,
                              node); // Recurse on left sub tree
    else
      return insert<DIM_NEXT>(key, value, node->right,
                              node); // Recurse on right sub tree
  }

  /**
   * Compare two keys on a dimension
   * @tparam DIM comparison dimension
   * @tparam Compare
   * @param a
   * @param b
   * @param compare
   * @return relationship of two keys on a dimension with the compare function
   */
  template <size_t DIM, typename Compare>
  static bool compareKey(const Key &a, const Key &b,
                         Compare compare = Compare()) {
    if (std::get<DIM>(a) != std::get<DIM>(b)) {
      return compare(std::get<DIM>(a), std::get<DIM>(b));
    }
    return compare(a, b);
  }

  /**
   * Compare two nodes on a dimension
   * @tparam DIM comparison dimension
   * @tparam Compare
   * @param a
   * @param b
   * @param compare
   * @return the minimum / maximum of two nodes
   */
  template <size_t DIM, typename Compare>
  static Node *compareNode(Node *a, Node *b, Compare compare = Compare()) {
    if (!a)
      return b;
    if (!b)
      return a;
    return compareKey<DIM, Compare>(a->key(), b->key(), compare) ? a : b;
  }

  /**
   * Find the minimum node on a dimension
   * @tparam DIM_CMP comparison dimension
   * @tparam DIM current dimension of node
   * @param node
   * @return the minimum node on a dimension
   */
  template <size_t DIM_CMP, size_t DIM> Node *findMin(Node *node) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (!node)
      return nullptr;
    auto min = findMin<DIM_CMP, DIM_NEXT>(node->left);
    if (DIM != DIM_CMP) {
      auto rightMin = findMin<DIM_CMP, DIM_NEXT>(node->right);
      min = compareNode<DIM_CMP, std::less<>>(min, rightMin);
    }
    return compareNode<DIM_CMP, std::less<>>(min, node);
  }

  /**
   * Find the maximum node on a dimension
   * @tparam DIM_CMP comparison dimension
   * @tparam DIM current dimension of node
   * @param node
   * @return the maximum node on a dimension
   */
  template <size_t DIM_CMP, size_t DIM> Node *findMax(Node *node) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (!node)
      return nullptr;
    auto max = findMax<DIM_CMP, DIM_NEXT>(node->right);
    if (DIM != DIM_CMP) {
      auto leftMax = findMax<DIM_CMP, DIM_NEXT>(node->left);
      max = compareNode<DIM_CMP, std::greater<>>(max, leftMax);
    }
    return compareNode<DIM_CMP, std::greater<>>(max, node);
  }

  template <size_t DIM> Node *findMinDynamic(size_t dim) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (dim >= KeySize)
      dim %= KeySize;
    if (dim == DIM)
      return findMin<DIM, 0>(root);
    return findMinDynamic<DIM_NEXT>(dim);
  }

  template <size_t DIM> Node *findMaxDynamic(size_t dim) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (dim >= KeySize)
      dim %= KeySize;
    if (dim == DIM)
      return findMax<DIM, 0>(root);
    return findMaxDynamic<DIM_NEXT>(dim);
  }

  /**
   * Erase a node with key (check the pseudocode in project description)
   * @tparam DIM current dimension of node
   * @param node
   * @param key
   * @return nullptr if node is erased, else the (probably) replaced node
   */
  template <size_t DIM> Node *erase(Node *node, const Key &key) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (!node)
      return nullptr;
    if (key == node->data.first) {
      if (!node->left && !node->right) {
        delete node;
        --treeSize;
        return nullptr;
      } else if (node->right) {
        auto minNode = findMin<DIM, DIM_NEXT>(node->right);
        const_cast<Key &>(node->data.first) = minNode->data.first;
        node->data.second = minNode->data.second;
        node->right = erase<DIM_NEXT>(node->right, minNode->data.first);
      } else {
        auto maxNode = findMax<DIM, DIM_NEXT>(node->left);
        const_cast<Key &>(node->data.first) = maxNode->data.first;
        node->data.second = maxNode->data.second;
        node->left = erase<DIM_NEXT>(node->left, maxNode->data.first);
      }
    } else if (compareKey<DIM, std::less<>>(key, node->data.first))
      node->left = erase<DIM_NEXT>(node->left, key);
    else
      node->right = erase<DIM_NEXT>(node->right, key);
    return node;
  }

  template <size_t DIM> Node *eraseDynamic(Node *node, size_t dim) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (dim >= KeySize) {
      dim %= KeySize;
    }
    if (dim == DIM)
      return erase<DIM>(node, node->key());
    return eraseDynamic<DIM_NEXT>(node, dim);
  }

  // Helper functions

  void deleteAllNodes(Node *node) {
    if (!node)
      return;
    else {
      deleteAllNodes(node->left);
      deleteAllNodes(node->right);
      delete node;
    }
    return;
  }

  void constructorHelper(Node *&thisNode, Node *thatNode, Node *thisParent) {
    if (!thatNode)
      return;
    // node -> left node -> right node
    thisNode =
        new Node(thatNode->data.first, thatNode->data.second, thisParent);
    constructorHelper(thisNode->left, thatNode->left, thisNode);
    constructorHelper(thisNode->right, thatNode->right, thisNode);
    return;
  }

  template <size_t DIM>
  void explicitHelper(std::vector<std::pair<Key, Value>> v, Node *&node,
                      Node *parent) {
    constexpr size_t DIM_NEXT = (DIM + 1) % KeySize;
    if (v.empty())
      return;
    // Locate median
    auto medianIt = v.begin() + (v.end() - v.begin() - 1) / 2;
    // Find median
    std::nth_element(
        v.begin(), medianIt, v.end(),
        [](const std::pair<Key, Value> &a, const std::pair<Key, Value> &b) {
          if (std::get<DIM>(a.first) != std::get<DIM>(b.first))
            return std::get<DIM>(a.first) < std::get<DIM>(b.first);
          return a.first < b.first;
        });
    // Construct node
    node = new Node(medianIt->first, medianIt->second, parent);
    ++treeSize;
    // Recurse on left and right sub tree
    explicitHelper<DIM_NEXT>(
        std::vector<std::pair<Key, Value>>(v.begin(), medianIt), node->left,
        node);
    explicitHelper<DIM_NEXT>(
        std::vector<std::pair<Key, Value>>(medianIt + 1, v.end()), node->right,
        node);
    return;
  }

public:
  KDTree() = default;

  /**
   * @param v we pass by value here because v need to be modified
   */
  explicit KDTree(std::vector<std::pair<Key, Value>> v) {
    if (v.empty())
      return;
    // Stable sort
    std::stable_sort(
        v.begin(), v.end(),
        [](const std::pair<Key, Value> &a, const std::pair<Key, Value> &b) {
          return a.first < b.first;
        });
    // Remove duplicated keys
    auto it = std::unique(v.rbegin(), v.rend(),
                          [](const std::pair<Key, Value> &a,
                             const std::pair<Key, Value> &b) {
                            return a.first == b.first;
                          })
                  .base();
    // Recurse
    explicitHelper<0>(std::vector<std::pair<Key, Value>>(it, v.end()), root,
                      nullptr);
    return;
  }

  KDTree(const KDTree &that) {
    treeSize = that.treeSize;
    constructorHelper(root, that.root, nullptr);
    return;
  }

  KDTree &operator=(const KDTree &that) {
    deleteAllNodes(root);
    treeSize = that.treeSize;
    constructorHelper(root, that.root, nullptr);
    return *this;
  }

  ~KDTree() {
    deleteAllNodes(root);
    return;
  }

  Iterator begin() {
    if (!root)
      return end();
    auto node = root;
    while (node->left)
      node = node->left;
    return Iterator(this, node);
  }

  Iterator end() { return Iterator(this, nullptr); }

  Iterator find(const Key &key) { return Iterator(this, find<0>(key, root)); }

  void insert(const Key &key, const Value &value) {
    insert<0>(key, value, root, nullptr);
  }

  template <size_t DIM> Iterator findMin() {
    return Iterator(this, findMin<DIM, 0>(root));
  }

  Iterator findMin(size_t dim) {
    return Iterator(this, findMinDynamic<0>(dim));
  }

  template <size_t DIM> Iterator findMax() {
    return Iterator(this, findMax<DIM, 0>(root));
  }

  Iterator findMax(size_t dim) {
    return Iterator(this, findMaxDynamic<0>(dim));
  }

  bool erase(const Key &key) {
    auto prevSize = treeSize;
    erase<0>(root, key);
    return prevSize > treeSize;
  }

  Iterator erase(Iterator it) {
    if (it == end())
      return it;
    auto node = it.node;
    if (!it.node->left && !it.node->right) {
      it.node = it.node->parent;
    }
    size_t depth = 0;
    auto temp = node->parent;
    while (temp) {
      temp = temp->parent;
      ++depth;
    }
    eraseDynamic<0>(node, depth % KeySize);
    return it;
  }

  size_t size() const { return treeSize; }
};
