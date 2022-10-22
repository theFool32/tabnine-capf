# tabnine-capf

A capf version of [company-tabnine](https://github.com/TommyX12/company-tabnine) which heavily relies on the [corfu](https://github.com/minad/corfu).
It is not tested throughly.

## Installation

### straight-use-package

Add following code to your configuration.
```emacs
(use-package tabnine-capf
  :after cape
  :straight (:host github :repo "theFool32/tabnine-capf" :files ("*.el" "*.sh"))
  :hook (kill-emacs . tabnine-capf-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-capf))
```
## Thanks

Thanks to the great work of [Tommy Xiang](https://github.com/TommyX12), [50ways2sayhard](https://github.com/50ways2sayhard/tabnine-capf), and [manateelazycat/lsp-bridge](https://github.com/manateelazycat/lsp-bridge/blob/master/core/tabnine.py).
