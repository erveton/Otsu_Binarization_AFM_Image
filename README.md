This R code provides a framework for obtaining all parameters related to the Otsu binarization method applied to AFM images. The output data includes: the optimal threshold, maximum separability index, percentage of pixels in valleys, percentage of pixels in peaks, mean height in valleys, mean height in peaks, effective mean height, and the separability index versus threshold curve. The primary input is the image matrix (in .txt format); however, the matrix dimensions in the code must be adjusted as needed, although the code is currently set for a 256 x 256 pixel matrix.

Otsu binarization is an image processing algorithm that automatically calculates the optimal threshold to separate image pixels into two classes: background and object. It achieves this by maximizing the variance between the two classes (or minimizing the variance within each class). This technique is particularly well-suited for bimodal histograms, such as the height histograms found in AFM images. Mathematical details of the method can be found in references [1, 2].

An illustration of the control panel in the RStudio environment is shown below. We also provide a purely random matrix for testing purposes.

<img width="1919" height="1017" alt="image" src="https://github.com/user-attachments/assets/4d372332-34b4-4d02-90aa-164fa3580989" />

References

[1] N. Otsu, A threshold selection method from gray-level histograms, in: IEEE Transactions on Systems, Man, and Cybernetics, Institute of Electrical
and Electronics Engineers (IEEE), 1979, pp. 62–66.

[2] E.P. Pinto, M.A. Pires, R.S. Matos, R.R. Zamora, R.P. Menezes, R.S. Araújo, T.M. de Souza, Lacunarity exponent and moran index: A complementary methodology to analyze AFM images and its application to chitosan films, Phys. A 581 (2021) 126192.
