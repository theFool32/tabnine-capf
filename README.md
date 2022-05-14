# tabnine-capf

A capf version of [company-tabnine](https://github.com/TommyX12/company-tabnine).

## Installation

### straight-use-package

Add following code to your configuration.
```emacs
(use-package tabnine-capf
  :after cape
  :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
  :hook (kill-emacs . tabnine-capf-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))
```

### manully
1. Install `tabnine-capf`.

   Clone or download this repository.

   Add to your load path:

   ```emacs
   (add-to-list 'load-path "<path-to-tabnine-capf>")
   (require 'tabnine-capf)
   ```

2. Add `tabnine-completion-at-point` to `completion-at-point-functions`
   ```emacs
   (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
   ```

3. Run `M-x company-tabnine-install-binary` to install the TabNine binary for your system.

### Auto-balance parentheses

TabNine can automatically balance parentheses, by removing and adding closing parentheses after the cursor. See the examples [here](https://github.com/zxqfl/TabNine/blob/master/HowToWriteAClient.md).

## Known Issues

- TabNine's local deep learning completion might be enabled by default. It is very CPU-intensive if your device can't handle it. You can check by typing "TabNine::config" in any buffer (your browser should then automatically open to TabNine's config page) and disable Deep TabNine Local (you will lose local deep learning completion).

## Thanks

Thanks to the great work of [Tommy Xiang](https://github.com/TommyX12).
