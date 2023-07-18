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
(setq-default frame-title-format
              '("" "%b%* (%f) %p/%P - GNU Emacs " emacs-version " at " system-name))

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
(global-set-key (kbd "C-x C-k k") 'magit-kill-this-buffer)

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
   '("3c68f48ea735abe65899f489271d11cbebbe87da7483acf9935ea4502efd0117"
     "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b"
     "3ea6a13b8119d69238e9651c05092d4491816b4d8066481e84e36901b6542089"
     "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70"
     "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8"
     "1127f29b2e4e4324fe170038cbd5d0d713124588a93941b38e6295a58a48b24f"
     "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736"
     "18624b2da7749af193a4eeaa7be1dc2abe94a97a8562ba69f5ee0f06d6dd156e"
     "47d5324dac28a85c1bb84b4c1dc3a8dc407cc7369db6e30d3244b16232b1eec4"
     "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27"
     "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a"
     "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866"
     "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233"
     "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9"
     "ebbd4bbb0f017cb09f7a3b1363b83dfde0c5f4970cda2705419457366cd2de91"
     "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f"
     "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e"
     "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58"
     "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016"
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
     "d422c7673d74d1e093397288d2e02c799340c5dabf70e87558b8e8faa3f83a6c"
     "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765"
     "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb"
     "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920"
     "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b"
     "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05"
     "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a"
     "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08"
     "a68624bd5c4bec879ee59cd3039531b6229766a8b8ed0e79eef2642f14dbda32"
     "afeb7b07dbc1a4cfadb24f3ef6c8cf5e63051bf76411779f03a0fe3aadc07768"
     "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2"
     "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241"
     "aaf783d4bfae32af3e87102c456fba8a85b79f6e586f9911795ea79055dee3bf"
     "9d9b2cf2ced850aad6eda58e247cf66da2912e0722302aaa4894274e0ea9f894"
     "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0"
     "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4"
     "b6f06081b007b57be61b82fb53f27315e2cf38fa690be50d6d63d2b62a408636"
     "995d0754b79c4940d82bd430d7ebecca701a08631ec46ddcd2c9557059758d33"
     "70b2d5330a8dd506accac4b51aaa7e43039503d000852d7d152aec2ce779d96d"
     "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b"
     "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52"
     "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336"
     "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca"
     "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed"
     "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428"
     "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0"
     "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34"
     "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3"
     "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37"
     "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22"
     "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3"
     "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3"
     "28818b9b1d9e58c4fb90825a1b07b0f38286a7d60bf0499bc2dea7eea7e36782"
     "cb30d82b05359203c8378638dec5ad6e37333ccdda9dee8b9fdf0c902e83fad7"
     "6b4f7bde1ce64ea4604819fe56ff12cda2a8c803703b677fdfdb603e8b1f8bcb"
     "6291d73aaeb6a3d7e455d85ca3865260a87afe5f492b6d0e2e391af2d3ea81dd"
     "01e0367d8c3249928a2e0ebc9807b2f791f81a0d2a7c8656e1fbf4b1dbaa404c"
     "6c0d748fb584ec4c8a0a1c05ce1ae8cde46faff5587e6de1cc59d3fc6618e164"
     "335ad769bcd7949d372879ec10105245255beec6e62213213835651e2eb0b8e0"
     "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8"
     "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7"
     "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20"
     "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe"
     "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7"
     "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d"
     "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b"
     "5f4dfda04fbf7fd55228266c8aab73953d3087cea7fd06dd7f8ff1e4a497c739"
     "2ae4b0c50dd49a5f74edeae3e49965bf8853954b63c5712a7967ea0a008ecd5b"
     "1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88"
     "305602d5eecdededb3bd7bf73b933d0ff0d5b756f02ae9ec8716cf7db3b6c641"
     "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4"
     "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3"
     "20a8ec387dde11cc0190032a9f838edcc647863c824eed9c8e80a4155f8c6037"
     "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf"
     "e01d36b3ca7991d21fba7f2708f0bfb587b61654898bf3dd92fb11c9fbf5a649"
     "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35"
     "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f"
     "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0"
     "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3"
     "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832"
     "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46"
     "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529"
     "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa"
     "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac"
     "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1"
     "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9"
     "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30"
     "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88"
     "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da"
     "332a945a80a202248b21963da38e842aa35b09d1b43c26144bd517943855fe8a"
     "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c"
     "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2"
     "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc"
     "91c008faf603a28d026957120a5a924a3c8fff0e12331abf5e04c0e9dd310c65"
     "0ed3d96a506b89c1029a1ed904b11b5adcebeb2e0c16098c99c0ad95cb124729"
     "b6c43bb2aea78890cf6bd4a970e6e0277d2daf0075272817ea8bb53f9c6a7f0a"
     "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db"
     "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1"
     "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739"
     "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222"
     "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7"
     "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea"
     "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1"
     "1711947b59ea934e396f616b81f8be8ab98e7d57ecab649a97632339db3a3d19"
     "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86"
     "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd"
     "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace"
     "44f5578eccb2cde3b196dfa86a298b75fe39ceff975110c091fa8c874c338b50"
     "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a"
     "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70"
     "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea"
     "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5"
     "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777"
     "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5"
     "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5"
     "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838"
     "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3"
     "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980"
     "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043"
     "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac"
     "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36"
     "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7"
     "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01"
     "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e"
     "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b"
     "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568"
     "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943"
     "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041"
     "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350"
     "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235"
     "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0"
     "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7"
     "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374"
     "9abe2b502db3ed511fea7ab84b62096ba15a3a71cdb106fd989afa179ff8ab8d"
     "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9"
     "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481"
     "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58"
     "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64"
     "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942"
     "53de65a1e7300e0f1a4f8bf317530a5008e9d06a0e2f8863b80dc56d77f844cf"
     "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7"
     "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc"
     "58c996beb973f7e988ee4fd21c367b7a5bbdb0622ddfbbd112672a7b4e3d3b81"
     "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2"
     "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f"
     "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760"
     "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da"
     "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1"
     "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41"
     "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727"
     "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f"
     "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041"
     "db86c52e18460fe10e750759b9077333f9414ed456dc94473f9cf188b197bc74"
     "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11"
     "5dbdb4a71a0e834318ae868143bb4329be492dd04bdf8b398fb103ba1b8c681a"
     default))

 '(display-time-mode t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(lua-mode magit yaml-mode cmake-mode dockerfile-mode twilight-anti-bright-theme badwolf-theme clues-theme soothe-theme flatui-dark-theme subatomic-theme tangotango-theme afternoon-theme kaolin-themes gruber-darker-theme alect-themes apropospriate-theme ample-theme cyberpunk-theme moe-theme material-theme dracula-theme gruvbox-theme monokai-theme spacemacs-theme color-theme-modern color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized zenburn-theme csharp-mode treemacs))
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(warning-suppress-types '((comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono derivative Powerline" :foundry "DAMA" :slant normal :weight normal :height 271 :width normal)))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
