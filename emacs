;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.ilproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.proj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.depproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.props\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.targets\\'" . xml-mode))

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default scroll-conservatively most-positive-fixnum)

(defun toggle-line-numbers-type ()
  "Toggle between absolute and relative line numbering."
  (if (not (equal display-line-numbers-type 'relative))
      (setq-default display-line-numbers-type 'relative)
    (setq-default display-line-numbers-type 'absolute))
  (global-display-line-numbers-mode 1))

(windmove-default-keybindings)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x x w") 'toggle-word-wrap)
(global-set-key (kbd "C-x x f") 'menu-set-font)
(global-set-key (kbd "C-<f7>") (lambda () (interactive) (toggle-line-numbers-type)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes '(deeper-blue))
 '(custom-safe-themes
   '("4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "9abe2b502db3ed511fea7ab84b62096ba15a3a71cdb106fd989afa179ff8ab8d" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942" "53de65a1e7300e0f1a4f8bf317530a5008e9d06a0e2f8863b80dc56d77f844cf" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "58c996beb973f7e988ee4fd21c367b7a5bbdb0622ddfbbd112672a7b4e3d3b81" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "db86c52e18460fe10e750759b9077333f9414ed456dc94473f9cf188b197bc74" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "5dbdb4a71a0e834318ae868143bb4329be492dd04bdf8b398fb103ba1b8c681a" default))
 '(display-time-mode t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(yaml-mode cmake-mode dockerfile-mode twilight-anti-bright-theme badwolf-theme clues-theme soothe-theme flatui-dark-theme subatomic-theme tangotango-theme afternoon-theme kaolin-themes gruber-darker-theme alect-themes apropospriate-theme ample-theme cyberpunk-theme moe-theme material-theme dracula-theme gruvbox-theme monokai-theme spacemacs-theme color-theme-modern color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized zenburn-theme csharp-mode treemacs))
 '(size-indication-mode t)
 '(warning-suppress-types '((comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono derivative Powerline" :foundry "DAMA" :slant normal :weight normal :height 263 :width normal)))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
