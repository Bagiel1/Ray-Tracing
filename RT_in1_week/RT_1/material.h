#ifndef MATERIAL_H
#define MATERIAL_H

#include "hittable.h"

class material{
    public:
        virtual ~material()= default;

        virtual bool scatter(
            const ray& r_in, const hit_record& rec, color& attenuation, ray& scattered
        ) const {
            return 0;
        }
};

class lambertian : public material{
    public:
        lambertian(const color& albedo): albedo(albedo) {}

        bool scatter(const ray& r_in, const hit_record& rec, color& attenuation, ray& scattered)
        const override{
            auto scatter_direction= rec.normal + random_unit_vector();
            scattered= ray(rec.p, scatter_direction);

            if(scatter_direction.near_zero()){
                scatter_direction= rec.normal;
            }

            attenuation= albedo;
            return true;
        }

    private:
        color albedo;
};

class metal : public material{
    public:
        metal(const color& albedo): albedo(albedo) {}

        bool scatter(const ray& r_in, const hit_record& rec, color& attenuation, ray& scattered)
        const override{
            Vec3 reflected= reflect(r_in.direction(), rec.normal);
            scattered= ray(rec.p, reflected);
            attenuation= albedo;
            return true;
        }

    private:
        color albedo;
};


#endif